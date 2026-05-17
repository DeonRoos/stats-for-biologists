# --- io + tokenizers ---
read_docx_text <- function(path) {
  tmp <- tempfile(); dir.create(tmp)
  unzip(path, files = "word/document.xml", exdir = tmp)
  f <- file.path(tmp, "word", "document.xml")
  if (!file.exists(f)) stop("word/document.xml not found in .docx")
  x <- readLines(f, warn = FALSE, encoding = "UTF-8") |> paste(collapse = " ")
  x |>
    gsub("<.*?>", " ", x = _) |>
    gsub("&amp;", "&", x = _) |>
    gsub("&lt;", "<", x = _) |>
    gsub("&gt;", ">", x = _) |>
    gsub("&#13;", " ", x = _) |>
    gsub("\\s+", " ", x = _) |>
    trimws()
}
tokenize_words <- function(text) {
  text |>
    tolower() |>
    gsub("[^[:alnum:]'\\s]", " ", x = _) |>
    gsub("\\s+", " ", x = _) |>
    strsplit("\\s+") |>
    (\(z) z[[1]])() |>
    (\(w) w[w != ""])()
}
tokenize_sentences <- function(text) {
  s <- gsub("\\s+", " ", text)
  s <- strsplit(s, "(?<=[\\.\\?\\!])\\s+", perl = TRUE)[[1]]
  trimws(s[nchar(s) > 0])
}

# --- vocab + ints ---
tok <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9\\s']", " ", x)
  strsplit(x, "\\s+") |> lapply(\(w) w[w != ""])
}
build_vocab <- function(tokens, min_count = 1) {
  all <- unlist(tokens, use.names = FALSE)
  tab <- sort(table(all), decreasing = TRUE)
  keep <- names(tab[tab >= min_count])
  idx <- setNames(seq_along(keep), keep)
  list(index = idx, itos = keep)
}
ints_from_tokens <- function(tokens, vocab) {
  lapply(tokens, \(s) unname(vocab$index[s[!is.na(vocab$index[s])]]))
}

# --- PPMI + SVD ---
cooc <- function(tokens, V, win = 2) {
  M <- matrix(0, V, V)
  for (s in tokens) {
    n <- length(s)
    if (n < 2) next
    for (i in seq_len(n)) {
      left <- max(1, i - win); right <- min(n, i + win)
      ctx <- s[setdiff(left:right, i)]
      if (length(ctx)) M[s[i], ctx] <- M[s[i], ctx] + 1
    }
  }
  M
}
ppmi <- function(M) {
  row_s <- rowSums(M); col_s <- colSums(M); tot <- sum(M)
  E <- (row_s %o% col_s) / max(tot, 1e-12)
  X <- log(pmax(M / pmax(E, 1e-12), 1e-12))
  X[X < 0] <- 0
  X
}
embed_ppmi_svd <- function(M, k = 50) {
  k <- min(k, nrow(M), ncol(M))
  s <- svd(M, nu = k, nv = k)
  W <- s$u[, seq_len(k), drop = FALSE] %*% diag(s$d[seq_len(k)]^0.5, nrow = k, ncol = k)
  W / sqrt(rowSums(W * W) + 1e-12)
}

# --- contexts -> next token ---
make_xy <- function(tokens_int, W, ctx = 2) {
  d <- ncol(W); V <- nrow(W)
  X <- matrix(0, 0, d); y <- integer()
  for (s in tokens_int) {
    if (length(s) <= ctx) next
    for (t in (ctx + 1):length(s)) {
      ids <- s[(t - ctx):(t - 1)]
      x <- colMeans(W[ids, , drop = FALSE])
      X <- rbind(X, x)
      y <- c(y, s[t])
    }
  }
  list(X = X, y = y, V = V)
}

# --- tiny MLP (3 hidden) ---
sigmoid <- function(x) 1 / (1 + exp(-x))
dsigmoid <- function(x) sigmoid(x) * (1 - sigmoid(x))
forward_mlp <- function(X, pars, d, V) {
  idx <- 0
  W1 <- matrix(pars[(idx + 1):(idx + d * 3)], nrow = d, ncol = 3); idx <- idx + d * 3
  b1 <- pars[(idx + 1):(idx + 3)]; idx <- idx + 3
  W2 <- matrix(pars[(idx + 1):(idx + 3 * V)], nrow = 3, ncol = V); idx <- idx + 3 * V
  b2 <- pars[(idx + 1):(idx + V)]
  H_in <- X %*% W1 + matrix(b1, nrow = nrow(X), ncol = 3, byrow = TRUE)
  H <- sigmoid(H_in)
  Z <- H %*% W2 + matrix(b2, nrow = nrow(X), ncol = V, byrow = TRUE)
  Zmax <- apply(Z, 1, max)
  expZ <- exp(Z - Zmax)
  P <- expZ / rowSums(expZ)
  list(P = P, H = H, H_in = H_in, W1 = W1, b1 = b1, W2 = W2, b2 = b2)
}
loss_grad_mlp <- function(pars, X, y, V) {
  n <- nrow(X)
  f <- forward_mlp(X, pars, ncol(X), V)
  if (n == 0) return(list(value = Inf, gradient = rep(0, length(pars))))
  idx <- cbind(seq_len(n), y)
  eps <- 1e-12
  loss <- -mean(log(pmax(f$P[idx], eps)))
  dZ <- f$P; dZ[idx] <- dZ[idx] - 1; dZ <- dZ / n
  dW2 <- t(f$H) %*% dZ
  db2 <- colSums(dZ)
  dH <- dZ %*% t(f$W2)
  dH_in <- dH * dsigmoid(f$H_in)
  dW1 <- t(X) %*% dH_in
  db1 <- colSums(dH_in)
  grad <- c(as.vector(dW1), db1, as.vector(dW2), db2)
  list(value = loss, gradient = grad)
}
init_pars <- function(d, V, sd = 0.05) rnorm(d * 3 + 3 + 3 * V + V, sd = sd)
predict_next <- function(ctx_words, vocab, W, pars) {
  ids <- unname(vocab$index[ctx_words]); ids <- ids[!is.na(ids)]
  if (!length(ids)) return(NA_integer_)
  x <- colMeans(W[ids, , drop = FALSE])
  f <- forward_mlp(matrix(x, 1), pars, ncol(W), nrow(W))
  which.max(drop(f$P))
}
generate <- function(prompt, max_new = 6, ctx = 2, vocab, W, pars) {
  words <- tok(prompt)[[1]]
  for (i in seq_len(max_new)) {
    nid <- predict_next(tail(words, ctx), vocab, W, pars)
    if (is.na(nid)) break
    words <- c(words, vocab$itos[[nid]])
  }
  paste(words, collapse = " ")
}

# --- corpus from a folder tree of .docx ---
root_dir <- "C:/006-BI3010/Workshops"
paths <- list.files(root_dir, pattern = "\\.docx$", recursive = TRUE, full.names = TRUE)
stopifnot(length(paths) > 0)
texts <- lapply(paths, \(p) try(read_docx_text(p), silent = TRUE))
texts <- Filter(\(z) !inherits(z, "try-error") && is.character(z) && length(z) == 1, texts)
stopifnot(length(texts) > 0)

sents <- unlist(lapply(texts, tokenize_sentences), use.names = FALSE)
tokens_chr <- lapply(sents, tokenize_words)
tokens_chr <- tokens_chr[lengths(tokens_chr) > 2]

vocab <- build_vocab(tokens_chr, min_count = 1)
tokens_int <- ints_from_tokens(tokens_chr, vocab)
V <- length(vocab$itos)

M <- cooc(tokens_int, V, win = 2)
Xppmi <- ppmi(M)
W <- embed_ppmi_svd(Xppmi, k = 16)

xy <- make_xy(tokens_int, W, ctx = 2)
stopifnot(nrow(xy$X) > 0)
d <- ncol(xy$X)

set.seed(1)
p0 <- init_pars(d, V, sd = 0.05)
fit <- optim(
  p0,
  fn = \(p) loss_grad_mlp(p, xy$X, xy$y, V)$value,
  gr = \(p) loss_grad_mlp(p, xy$X, xy$y, V)$gradient,
  method = "BFGS",
  control = list(maxit = 2000, reltol = 1e-8)
)

loss <- loss_grad_mlp(fit$par, xy$X, xy$y, V)$value
loss

generate("linear model", max_new = 3, ctx = 2, vocab = vocab, W = W, pars = fit$par)
generate("models are", max_new = 8, ctx = 2, vocab = vocab, W = W, pars = fit$par)
