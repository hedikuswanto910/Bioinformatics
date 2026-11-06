# =============================================================================
# siapkan_data.R - Menyiapkan data dari file Excel LKE (jalankan sekali/tahun)
# -----------------------------------------------------------------------------
# Dari folder lke-zi-dashboard, jalankan di terminal:
#
#   Rscript scripts/siapkan_data.R --rubrik  "Indeks_Revisi_FK_UNHAS.xlsx"
#   Rscript scripts/siapkan_data.R --riwayat "Indeks_Revisi_FK_UNHAS.xlsx"
#   Rscript scripts/siapkan_data.R --isian   "Indeks_Revisi_FK_UNHAS.xlsx" --tahun 2026
#
# atau dari konsol RStudio:
#   source("scripts/siapkan_data.R"); siapkan(rubrik = "file.xlsx")
#
# --rubrik  : membangun ulang data/rubrik_lke.csv bila Kementerian mengubah
#             template (butuh template dengan kolom penanda PARENT/SUB).
# --riwayat : menyimpan jawaban & catatan evaluator ke data/lokal (tidak ikut
#             ke GitHub) untuk ditampilkan di setiap butir.
# --isian   : mengisi database dengan jawaban/narasi/bukti dari file sebagai
#             draf awal (butir yang sudah diisi tidak ditimpa).
# =============================================================================

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")

siapkan <- function(rubrik = NULL, riwayat = NULL, isian = NULL, tahun = KONFIG$tahun) {
  if (!is.null(rubrik)) {
    rb <- bangun_rubrik(rubrik)
    utils::write.csv(rb, KONFIG$file_rubrik, row.names = FALSE, fileEncoding = "UTF-8")
    message(sprintf("Rubrik: %d butir + %d rincian -> %s", sum(rb$tipe == "butir"), sum(rb$tipe == "rincian"), KONFIG$file_rubrik))
  }
  rb <- muat_rubrik()
  if (!is.null(riwayat)) {
    rw <- baca_riwayat(riwayat, rb)
    dir.create(KONFIG$folder_lokal, showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(rw, file_riwayat(), row.names = FALSE, fileEncoding = "UTF-8")
    message(sprintf("Riwayat: %d baris -> %s", nrow(rw), file_riwayat()))
  }
  if (!is.null(isian)) {
    con <- buka_db(); on.exit(DBI::dbDisconnect(con))
    df <- baca_isian_excel(isian, rb, tahun)
    df <- df[nzchar(df$jawaban) | nzchar(df$narasi) | nzchar(df$bukti), ]
    sudah <- baca_isian(con, tahun)$kode
    df <- df[!(df$kode %in% sudah), ]
    simpan_isian(con, df, tahun, oleh = "impor", aksi = paste("impor", basename(isian)))
    message(sprintf("Isian: %d baris diimpor untuk tahun %d", nrow(df), tahun))
  }
  invisible(TRUE)
}

if (!interactive()) {
  a <- commandArgs(trailingOnly = TRUE)
  ambil <- function(flag) { i <- match(flag, a); if (is.na(i) || i == length(a)) NULL else a[i + 1] }
  if (!length(a)) {
    cat("Pemakaian: Rscript scripts/siapkan_data.R [--rubrik file] [--riwayat file] [--isian file] [--tahun 2026]\n")
  } else {
    siapkan(rubrik = ambil("--rubrik"), riwayat = ambil("--riwayat"), isian = ambil("--isian"),
            tahun = as.integer(ambil("--tahun") %||% KONFIG$tahun))
  }
}
