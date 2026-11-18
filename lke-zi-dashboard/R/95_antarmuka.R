# =============================================================================
# 95_antarmuka.R - Animasi (anime.js, Motion) dan grafik bergaya shadcn/ui
#                  charts (Recharts) untuk Shiny
# -----------------------------------------------------------------------------
# Seluruh kode JavaScript dibundel di www/vendor/zi-ui.min.js (sumber di
# folder js/). R hanya menyiapkan spesifikasi grafik; menjalankan aplikasi
# TIDAK memerlukan Node.js.
#
# Spesifikasi grafik (list) yang dikirim ke peramban:
#   judul      : teks untuk pembaca layar
#   data       : data frame; kolom `label` (dan opsional `pendek` untuk layar
#                sempit) serta satu kolom per seri
#   seri       : list(list(key, label, warna), ...)
#   tumpuk     : TRUE untuk batang bertumpuk
#   satuan     : mis. "%" ; desimal : jumlah angka desimal di tooltip
#   domain     : rentang sumbu nilai, mis. c(0, 100)
#   label_nilai: TRUE untuk menampilkan angka di ujung batang
# =============================================================================

# Palet grafik (variabel CSS dipakai oleh komponen grafik di peramban)
WARNA_GRAFIK <- list(
  klaim = "var(--chart-1)", tpi = "var(--chart-2)", unit_lalu = "var(--chart-3)",
  `belum diisi` = "var(--status-belum)", merah = "var(--status-merah)",
  kuning = "var(--status-kuning)", hijau = "var(--status-hijau)")

zi_chart_output <- function(id, tinggi_min = "14rem") {
  htmltools::div(id = id, class = "zi-chart", style = sprintf("min-height:%s;", tinggi_min))
}

render_zi_chart <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  shiny::createRenderFunction(func, function(value, session, name, ...) value, outputFunc = zi_chart_output)
}

.label_komponen <- function(no, area_kode, nama) paste0("Zona ", no, " (", area_kode, ") ", nama)
.label_pendek   <- function(no, area_kode) paste0("Zona ", no, " (", area_kode, ")")

# Perbandingan skor klaim dengan penilaian terdahulu per komponen (% bobot)
spek_grafik_komponen <- function(rubrik, skor, riwayat = NULL) {
  kp <- skor$komponen
  d <- data.frame(label = .label_komponen(kp$komponen_no, kp$area_kode, kp$komponen),
                  pendek = .label_pendek(kp$komponen_no, kp$area_kode),
                  klaim = round(100 * kp$skor / kp$bobot_komponen, 1), stringsAsFactors = FALSE)
  seri <- list()
  sumber_lalu <- c(jawaban_unit = "unit_lalu", jawaban_tpi = "tpi")
  label_lalu  <- c(unit_lalu = "Klaim unit (lalu)", tpi = "TPI (lalu)")
  if (!is.null(riwayat)) {
    for (kol in names(sumber_lalu)) {
      if (!kol %in% names(riwayat) || !any(nzchar(riwayat[[kol]]))) next
      k2 <- hitung_skor(rubrik, stats::setNames(riwayat[[kol]], riwayat$kode), kosong_nol = FALSE)$komponen
      kunci <- sumber_lalu[[kol]]
      d[[kunci]] <- round(100 * k2$skor / k2$bobot_komponen, 1)[match(d$label, .label_komponen(k2$komponen_no, k2$area_kode, k2$komponen))]
      seri[[length(seri) + 1]] <- list(key = kunci, label = label_lalu[[kunci]], warna = WARNA_GRAFIK[[kunci]])
    }
  }
  seri[[length(seri) + 1]] <- list(key = "klaim", label = "Klaim saat ini", warna = WARNA_GRAFIK$klaim)
  d <- d[order(d$label), ]
  list(judul = "Perbandingan skor klaim dan penilaian evaluator terdahulu per komponen",
       data = d, seri = seri, satuan = "%", desimal = 1, domain = c(0, 100), label_nilai = FALSE)
}

# Distribusi status validasi butir per komponen (batang bertumpuk)
spek_grafik_status <- function(status, butir) {
  st <- merge(status, unique(butir[, c("kode", "zona", "nama_zona")]), by = "kode")
  st$label  <- ifelse(st$zona == "H", "Hasil", paste0(st$nama_zona, " (", substr(st$area, 1, 1), ") ", st$komponen))
  st$pendek <- ifelse(st$zona == "H", "Hasil", paste0(st$nama_zona, " (", substr(st$area, 1, 1), ")"))
  tingkat <- c("hijau", "kuning", "merah", "belum diisi")
  tb <- as.data.frame.matrix(table(st$label, factor(st$status, tingkat)))
  d <- data.frame(label = rownames(tb), pendek = st$pendek[match(rownames(tb), st$label)], stringsAsFactors = FALSE)
  kunci <- c(hijau = "hijau", kuning = "kuning", merah = "merah", `belum diisi` = "belum")
  for (t in tingkat) d[[kunci[[t]]]] <- tb[[t]]
  nama <- c(hijau = "Hijau", kuning = "Kuning", merah = "Merah", `belum diisi` = "Belum diisi")
  list(judul = "Jumlah butir per status validasi pada setiap komponen", data = d, tumpuk = TRUE,
       seri = lapply(tingkat, function(t) list(key = kunci[[t]], label = nama[[t]], warna = WARNA_GRAFIK[[t]])),
       satuan = "", desimal = 0, label_nilai = FALSE, tinggi_min = 200)
}

# CSS komponen grafik (meniru tampilan shadcn/ui charts dengan palet marun-emas)
CSS_GRAFIK <- isi_warna("
:root { --chart-1: {{marun}}; --chart-2: {{emas}}; --chart-3: #E8B4AE;
        --status-hijau: #1E7B4A; --status-kuning: #D99A00; --status-merah: #B42318; --status-belum: #A8A29E;
        --zi-teks: #3A1010; --zi-teks-redup: #7A6A6A; --zi-grid: #EADADA; --zi-kursor: rgba(122,31,31,.06); }
.zi-chart { width: 100%; position: relative; }
.zi-grafik { width: 100%; }
.zi-grafik .recharts-surface { overflow: visible; }
.zi-grafik .recharts-cartesian-axis-tick-value { font-family: inherit; }
.zi-legenda { display: flex; flex-wrap: wrap; gap: .35rem .9rem; justify-content: center; margin: 0 0 .5rem; font-size: .8rem; color: var(--zi-teks); }
.zi-legenda-item { display: inline-flex; align-items: center; gap: .4rem; }
.zi-legenda-titik { width: .65rem; height: .65rem; border-radius: 3px; display: inline-block; }
.zi-tooltip { min-width: 10rem; background: #fff; border: 1px solid #EADADA; border-radius: .6rem; padding: .5rem .65rem;
              box-shadow: 0 10px 24px -8px rgba(78,7,0,.25); font-size: .78rem; color: var(--zi-teks); }
.zi-tooltip-judul { font-weight: 600; margin-bottom: .3rem; }
.zi-tooltip-baris { display: grid; grid-template-columns: .65rem 1fr auto; align-items: center; gap: .45rem; line-height: 1.6; }
.zi-tooltip-indikator { width: .65rem; height: .65rem; border-radius: 2px; }
.zi-tooltip-label { color: var(--zi-teks-redup); }
.zi-tooltip-nilai { font-weight: 600; font-variant-numeric: tabular-nums; text-align: right; }
.zi-grafik-catatan, .zi-grafik-galat { font-size: .8rem; color: var(--zi-teks-redup); margin-top: .4rem; }
@media (prefers-reduced-motion: reduce) { .kpi-kartu, .card { transition: none !important; } }
")

zi_ui_head <- function() {
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(CSS_GRAFIK)),
    htmltools::tags$style(htmltools::HTML(CSS_PIL)),
    htmltools::tags$meta(name = "zi-transisi-tab", content = KONFIG$transisi_tab_ms),
    htmltools::tags$script(src = paste0("vendor/zi-ui.min.js?v=", as.integer(file.mtime("www/vendor/zi-ui.min.js"))), defer = NA))
}

# -----------------------------------------------------------------------------
# Filter status butir berbentuk pil (kontrol segmen) dengan penanda meluncur.
# Tetap memakai radioButtons Shiny sehingga input$f_status tidak berubah;
# tampilan dan penanda meluncurnya diatur CSS_PIL dan zi-ui.min.js.
# -----------------------------------------------------------------------------
PIL_STATUS <- c(Semua = "Semua", Merah = "merah", Kuning = "kuning", Hijau = "hijau", Belum = "belum diisi")

pil_status_nama <- function(jumlah = NULL) {
  lapply(names(PIL_STATUS), function(lbl) {
    v <- PIL_STATUS[[lbl]]
    n <- if (is.null(jumlah) || is.null(jumlah[[v]])) "\u2013" else format(jumlah[[v]])
    htmltools::tags$span(class = paste0("pil-isi pil-", sub(" .*", "", v)),
      htmltools::tags$span(class = "pil-angka", n), htmltools::tags$span(class = "pil-label", lbl))
  })
}

pil_status_input <- function(id, label = "Filter status butir") {
  htmltools::div(class = "pil-status",
    shiny::radioButtons(id, label, choiceNames = pil_status_nama(), choiceValues = unname(PIL_STATUS),
                        selected = "Semua", inline = TRUE))
}

jumlah_status <- function(status) {
  st <- status$status
  c(Semua = length(st), merah = sum(st == "merah"), kuning = sum(st == "kuning"),
    hijau = sum(st == "hijau"), `belum diisi` = sum(st == "belum diisi"))
}

CSS_PIL <- isi_warna("
/* --- filter status berbentuk pil --- */
.pil-status { margin-bottom: .75rem; container-type: inline-size; }
.pil-status .control-label { position: absolute; width: 1px; height: 1px; overflow: hidden; clip: rect(0 0 0 0); white-space: nowrap; }
.pil-status .shiny-options-group { position: relative; isolation: isolate; display: grid; grid-template-columns: repeat(auto-fit, minmax(2.75rem, 1fr));
  gap: 2px; padding: 3px; border-radius: 12px; background: rgba(122,31,31,.08); }
.pil-status label.radio-inline { position: relative; display: flex; align-items: center; justify-content: center; min-height: 2.75rem;
  margin: 0 !important; padding: .3rem .1rem !important; border-radius: 9px; cursor: pointer; text-align: center; transition: background .15s ease; }
/* Bootstrap memberi ruang kiri untuk bulatan radio; tidak diperlukan pada pil */
.pil-status label.radio-inline:hover { background: rgba(255,255,255,.55); }
.pil-status input[type=radio] { position: absolute; inset: 0; width: 100%; height: 100%; margin: 0; opacity: 0; cursor: pointer; }
.pil-status label:has(input:focus-visible) { outline: 2px solid {{emas}}; outline-offset: 1px; }
.pil-isi { display: flex; flex-direction: column; align-items: center; line-height: 1.15; pointer-events: none; }
.pil-angka { font-weight: 700; font-size: .95rem; font-variant-numeric: tabular-nums; color: #3A1010; }
.pil-label { display: inline-flex; align-items: center; gap: .25rem; font-size: .7rem; color: #7A6A6A; white-space: nowrap; }
.pil-label::before { content: ''; width: .45rem; height: .45rem; border-radius: 50%; background: var(--warna-pil); }
.pil-Semua { --warna-pil: {{marun}}; } .pil-merah { --warna-pil: #B42318; } .pil-kuning { --warna-pil: #D99A00; }
.pil-hijau { --warna-pil: #1E7B4A; } .pil-belum { --warna-pil: #A8A29E; }
.pil-status label:has(input:checked) .pil-angka { color: {{marun}}; }
.pil-status label:has(input:checked) .pil-label { color: #3A1010; font-weight: 600; }
.pil-penanda { position: absolute; top: 0; left: 0; z-index: -1; border-radius: 9px; background: #fff; pointer-events: none;
  box-shadow: 0 1px 2px rgba(78,7,0,.12), 0 4px 12px rgba(78,7,0,.12); }
/* tidak cukup ruang untuk 5 pil selebar >= 44 px: susun 3 + 2 */
@container (max-width: 14.75rem) { .pil-status .shiny-options-group { grid-template-columns: repeat(3, minmax(0, 1fr)); } }
/* tanpa JavaScript: pil terpilih tetap ditandai */
.pil-status:not(.siap) label:has(input:checked) { background: #fff; box-shadow: 0 1px 2px rgba(78,7,0,.12); }

/* --- penanda meluncur di ribbon menu (desktop) --- */
.penanda-ribbon { display: none; }
@media (min-width: 1024px) {
  #nav { position: relative; isolation: isolate; }
  .penanda-ribbon { display: block; position: absolute; top: 0; left: 0; z-index: -1; pointer-events: none; background: rgba(0,0,0,.22); }
  .penanda-ribbon::after { content: ''; position: absolute; left: 0; right: 0; bottom: 0; height: 4px; border-radius: 4px 4px 0 0; background: {{emas}}; }
  #nav.ada-penanda .nav-link.active { background: transparent !important; }
  #nav.ada-penanda .nav-link.active:not(.dropdown-toggle)::after { opacity: 0; }
}
")
