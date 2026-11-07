# =============================================================================
# 40_penyimpanan.R - Penyimpanan isian di SQLite (satu file: data/lokal/zi.sqlite)
# -----------------------------------------------------------------------------
# Tabel:
#   isian  : jawaban, narasi, bukti per butir per tahun (versi terakhir)
#   jejak  : riwayat setiap penyimpanan (siapa, kapan, apa) - untuk audit
#   log_ai : hasil validasi/draf Claude beserta token & estimasi biaya
# Cadangkan file zi.sqlite secara berkala (cukup salin filenya).
# =============================================================================

buka_db <- function(path = KONFIG$file_db) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS isian (
      kode TEXT, tahun INTEGER, jawaban TEXT, narasi TEXT, bukti TEXT,
      pic TEXT, diperbarui TEXT, PRIMARY KEY (kode, tahun))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS jejak (
      waktu TEXT, tahun INTEGER, kode TEXT, oleh TEXT, aksi TEXT, jawaban TEXT)")
  # Register tindakan korektif (ISO 9001:2015 klausul 10.2; ISO 37001:2016 klausul 10.1)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS capa (
      id INTEGER PRIMARY KEY AUTOINCREMENT, tahun INTEGER, kode TEXT, sumber TEXT,
      temuan TEXT, akar_masalah TEXT, tindakan TEXT, pic TEXT, tenggat TEXT,
      status TEXT, bukti_penutupan TEXT, dibuat TEXT, diperbarui TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS log_ai (
      waktu TEXT, tahun INTEGER, kode TEXT, jenis TEXT, model TEXT,
      token_in INTEGER, token_out INTEGER, biaya REAL, hasil TEXT)")
  con
}

baca_isian <- function(con, tahun = KONFIG$tahun) {
  DBI::dbGetQuery(con, "SELECT kode, jawaban, narasi, bukti, pic, diperbarui FROM isian WHERE tahun = ?",
                  params = list(as.integer(tahun)))
}

# Simpan satu atau beberapa baris isian (data.frame: kode, jawaban, narasi, bukti)
simpan_isian <- function(con, df, tahun = KONFIG$tahun, oleh = "", aksi = "simpan") {
  if (nrow(df) == 0) return(invisible(0))
  waktu <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  isi <- function(x) { x <- as.character(x %||% ""); x[is.na(x)] <- ""; x }
  df$narasi <- if (is.null(df$narasi)) "" else isi(df$narasi)
  df$bukti  <- if (is.null(df$bukti)) "" else isi(df$bukti)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con,
      "INSERT INTO isian (kode, tahun, jawaban, narasi, bukti, pic, diperbarui)
       VALUES (?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(kode, tahun) DO UPDATE SET jawaban = excluded.jawaban, narasi = excluded.narasi,
         bukti = excluded.bukti, pic = excluded.pic, diperbarui = excluded.diperbarui",
      params = list(df$kode, rep(as.integer(tahun), nrow(df)), isi(df$jawaban), df$narasi, df$bukti,
                    rep(oleh, nrow(df)), rep(waktu, nrow(df))))
    DBI::dbExecute(con, "INSERT INTO jejak VALUES (?, ?, ?, ?, ?, ?)",
                   params = list(rep(waktu, nrow(df)), rep(as.integer(tahun), nrow(df)), df$kode,
                                 rep(oleh, nrow(df)), rep(aksi, nrow(df)), isi(df$jawaban)))
  })
  invisible(nrow(df))
}

catat_ai <- function(con, kode, jenis, h, tahun = KONFIG$tahun) {
  DBI::dbExecute(con, "INSERT INTO log_ai VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), as.integer(tahun), kode, jenis,
                  h$model %||% "", as.integer(h$token_in %||% 0), as.integer(h$token_out %||% 0),
                  as.numeric(h$biaya %||% 0), h$teks %||% (h$pesan %||% "")))
}

baca_log_ai <- function(con, tahun = KONFIG$tahun) {
  DBI::dbGetQuery(con, "SELECT * FROM log_ai WHERE tahun = ? ORDER BY waktu DESC", params = list(as.integer(tahun)))
}

baca_jejak <- function(con, tahun = KONFIG$tahun, n = 500) {
  DBI::dbGetQuery(con, "SELECT * FROM jejak WHERE tahun = ? ORDER BY waktu DESC LIMIT ?", params = list(as.integer(tahun), n))
}

# Riwayat penilaian evaluator disimpan sebagai CSV di data/lokal
file_riwayat <- function() file.path(KONFIG$folder_lokal, "riwayat_penilaian.csv")
muat_riwayat <- function() {
  f <- file_riwayat()
  if (!file.exists(f)) return(NULL)
  rw <- utils::read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8", colClasses = "character")
  rw[is.na(rw)] <- ""
  rw
}

# -----------------------------------------------------------------------------
# Register tindakan korektif (CAPA)
# Status mengikuti siklus: Terbuka -> Dalam proses -> Selesai -> Terverifikasi efektif
# -----------------------------------------------------------------------------
STATUS_CAPA <- c("Terbuka", "Dalam proses", "Selesai", "Terverifikasi efektif")
SUMBER_CAPA <- c("Validasi otomatis", "Catatan evaluator (TPI/TPSK/TP PTN)", "Pra-penilaian Claude", "Audit internal", "Lainnya")

baca_capa <- function(con, tahun = KONFIG$tahun) {
  DBI::dbGetQuery(con, "SELECT * FROM capa WHERE tahun = ? ORDER BY CASE status WHEN 'Terbuka' THEN 0
                         WHEN 'Dalam proses' THEN 1 WHEN 'Selesai' THEN 2 ELSE 3 END, tenggat", params = list(as.integer(tahun)))
}

simpan_capa <- function(con, data, tahun = KONFIG$tahun, id = NULL) {
  waktu <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  isi <- function(x) { x <- as.character(x %||% ""); if (is.na(x)) "" else x }
  if (is.null(id) || is.na(id)) {
    DBI::dbExecute(con, "INSERT INTO capa (tahun, kode, sumber, temuan, akar_masalah, tindakan, pic, tenggat, status,
                         bukti_penutupan, dibuat, diperbarui) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
      params = list(as.integer(tahun), isi(data$kode), isi(data$sumber), isi(data$temuan), isi(data$akar_masalah),
                    isi(data$tindakan), isi(data$pic), isi(data$tenggat), isi(data$status %||% "Terbuka"),
                    isi(data$bukti_penutupan), waktu, waktu))
  } else {
    DBI::dbExecute(con, "UPDATE capa SET kode=?, sumber=?, temuan=?, akar_masalah=?, tindakan=?, pic=?, tenggat=?,
                         status=?, bukti_penutupan=?, diperbarui=? WHERE id=?",
      params = list(isi(data$kode), isi(data$sumber), isi(data$temuan), isi(data$akar_masalah), isi(data$tindakan),
                    isi(data$pic), isi(data$tenggat), isi(data$status), isi(data$bukti_penutupan), waktu, as.integer(id)))
  }
}

# Validasi isian CAPA sebelum disimpan: tindakan korektif yang dapat diverifikasi
# membutuhkan akar masalah, PIC, dan tenggat; status "Terverifikasi efektif"
# membutuhkan bukti penutupan.
periksa_capa <- function(data) {
  m <- character(0)
  if (!nzchar(trimws(data$temuan %||% ""))) m <- c(m, "Uraian temuan wajib diisi.")
  if (!nzchar(trimws(data$tindakan %||% ""))) m <- c(m, "Tindakan korektif wajib diisi.")
  if (!nzchar(trimws(data$pic %||% ""))) m <- c(m, "Penanggung jawab (PIC) wajib diisi.")
  if (!nzchar(trimws(data$tenggat %||% ""))) m <- c(m, "Tenggat wajib diisi.")
  if ((data$status %||% "") %in% c("Selesai", "Terverifikasi efektif") && !nzchar(trimws(data$akar_masalah %||% "")))
    m <- c(m, "Akar masalah harus dirumuskan sebelum tindakan dinyatakan selesai.")
  if (identical(data$status, "Terverifikasi efektif") && !nzchar(trimws(data$bukti_penutupan %||% "")))
    m <- c(m, "Status 'Terverifikasi efektif' memerlukan bukti penutupan.")
  m
}
