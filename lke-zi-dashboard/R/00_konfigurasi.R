# =============================================================================
# 00_konfigurasi.R - Pengaturan umum aplikasi Dashboard LKE Zona Integritas
# -----------------------------------------------------------------------------
# Ubah nilai di sini (atau lewat variabel lingkungan di file .Renviron)
# tanpa perlu menyentuh file lain.
# =============================================================================

KONFIG <- list(
  # Tahun penilaian yang sedang dikerjakan. Bukti dianggap "sesuai periode"
  # bila menyebut tahun ini atau tahun sebelumnya.
  tahun = as.integer(Sys.getenv("ZI_TAHUN", format(Sys.Date(), "%Y"))),

  # Nama unit kerja (muncul di judul dan dokumen ekspor)
  unit = Sys.getenv("ZI_UNIT", "Fakultas Kedokteran Universitas Hasanuddin"),

  # Lokasi file. Folder data/lokal TIDAK ikut ke GitHub (lihat .gitignore)
  # karena berisi data internal unit.
  file_rubrik   = "data/rubrik_lke.csv",
  folder_lokal  = "data/lokal",
  file_db       = "data/lokal/zi.sqlite",
  folder_unggah = "data/lokal/unggahan",

  # --- Integrasi Claude (opsional) -------------------------------------------
  # Kunci API dibaca dari variabel lingkungan ANTHROPIC_API_KEY.
  # Tanpa kunci, semua fitur lain tetap berjalan; hanya tombol Claude yang mati.
  model        = Sys.getenv("ZI_CLAUDE_MODEL", "claude-opus-5-5"),
  effort       = Sys.getenv("ZI_CLAUDE_EFFORT", "medium"),  # low/medium/high/xhigh/max
  api_versi    = "2026-10-21",
  # Harga per 1 juta token (USD) untuk estimasi biaya di layar
  harga_input  = 4,
  harga_output = 20,
  # Batas ukuran total PDF yang dikirim ke Claude dalam satu permintaan
  maks_mb_pdf  = 25
)

# Ambang aturan validasi berbasis aturan (bisa disesuaikan Tim ZI)
ATURAN <- list(
  narasi_min_karakter = 300,  # di bawah ini dianggap narasi terlalu tipis
  judul_bukti_min     = 10    # judul dokumen bukti minimal sekian karakter
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
