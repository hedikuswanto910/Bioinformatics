# =============================================================================
# 80_tampilan.R - Tema warna, gaya (CSS), header, dan elemen tampilan
# -----------------------------------------------------------------------------
# Tema mengacu pada situs med.unhas.ac.id: ribbon merah, kartu merah marun,
# aksen emas/oranye, judul biru tua. Warna diambil dari tangkapan layar situs.
# Ganti nilai di daftar WARNA untuk mengubah seluruh tema.
# =============================================================================

WARNA <- list(
  merah    = "#A80C0D",   # ribbon menu
  marun    = "#690A08",   # kartu
  marun2   = "#8E0F12",   # gradasi kartu (lebih terang)
  gelap    = "#4E0700",   # bayangan/gradasi tergelap
  emas     = "#F9B03D",   # aksen
  oranye   = "#EFA119",
  judul    = "#162232",   # teks judul (biru tua)
  latar    = "#F8F1EF",   # latar halaman (krem kemerahan, bukan putih)
  muda     = "#FDECEA",   # latar lembut (chip kode, kotak keterangan)
  samping  = "#FFF9F7"    # panel samping
)

# Ganti {{nama}} di teks CSS/JS dengan warna dari daftar WARNA
isi_warna <- function(teks) {
  for (n in names(WARNA)) teks <- gsub(paste0("{{", n, "}}"), WARNA[[n]], teks, fixed = TRUE)
  teks
}

tema_aplikasi <- function() {
  bslib::bs_theme(
    version = 5,
    primary = WARNA$merah, secondary = WARNA$marun, success = "#2E8B57",
    warning = WARNA$emas, danger = "#C62828", info = "#2F6FA8",
    red = WARNA$merah, orange = WARNA$oranye, yellow = WARNA$emas,
    "headings-color" = WARNA$judul,
    base_font = bslib::font_collection(bslib::font_google("Roboto", local = FALSE),
                                       "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
    "border-radius" = "0.65rem"
  )
}

CSS_APLIKASI <- isi_warna("
body {
  background-color: {{latar}};
  background-image:
    radial-gradient(circle at 0% 0%, rgba(168,12,13,.10) 0%, transparent 35%),
    radial-gradient(circle at 100% 100%, rgba(249,176,61,.16) 0%, transparent 35%),
    radial-gradient(rgba(105,10,8,.07) 1px, transparent 1px);
  background-size: auto, auto, 22px 22px;
  background-attachment: fixed;
  color: #2b2b2b;
}
h1, h2, h3, h4, h5 { color: {{judul}}; font-weight: 700; }

/* Kartu: kepala merah marun seperti kartu di situs fakultas */
.card { border: 0; border-radius: 12px; box-shadow: 0 6px 18px rgba(78,7,0,.14); background: #fff; }
.card > .card-header { background: linear-gradient(120deg, {{marun2}} 0%, {{marun}} 60%, {{gelap}} 100%);
                       color: #fff; font-weight: 600; border-bottom: 3px solid {{emas}};
                       border-radius: 12px 12px 0 0 !important; }
.card > .card-header .btn { color: #fff; border-color: rgba(255,255,255,.6); }
.card > .card-header .btn:hover { background: rgba(255,255,255,.15); }

/* Kartu KPI: merah marun, rata tengah (ikon kecil -> angka -> keterangan),
   mengikuti kartu 'FKUH Dalam Angka' di situs fakultas */
.kpi-kartu { display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center;
  gap: .35rem; padding: 1.1rem 1rem 1.2rem; min-height: 9rem; border-radius: 12px; color: #fff;
  background: linear-gradient(135deg, {{marun2}} 0%, {{marun}} 55%, {{gelap}} 100%);
  border-bottom: 4px solid {{emas}}; box-shadow: 0 8px 20px rgba(78,7,0,.28);
  transition: transform .2s ease, box-shadow .2s ease; }
.kpi-kartu:hover { transform: translateY(-3px); box-shadow: 0 12px 26px rgba(78,7,0,.35); }
.kpi-kartu .kpi-ikon { display: inline-flex; align-items: center; justify-content: center; width: 2.6rem; height: 2.6rem;
  border-radius: 50%; background: rgba(249,176,61,.16); color: {{emas}}; font-size: 1.2rem; }
.kpi-kartu .kpi-ikon svg { width: 1.2rem; height: 1.2rem; fill: currentColor; }
.kpi-kartu .kpi-nilai { font-size: clamp(1.5rem, 1.2rem + 1vw, 2.1rem); font-weight: 700; line-height: 1.1; color: #fff; }
.kpi-kartu .kpi-nilai .shiny-text-output { display: inline; }
.kpi-kartu .kpi-judul { font-size: .9rem; color: rgba(255,255,255,.85); line-height: 1.3; }

/* Tata letak dengan panel samping */
.bslib-sidebar-layout { border: 0 !important; border-radius: 12px; background: #fff !important;
                        box-shadow: 0 6px 18px rgba(78,7,0,.14); }
.bslib-sidebar-layout > .sidebar { background: {{samping}} !important; border-right: 1px solid #f0dcd8 !important; }
.bslib-sidebar-layout > .main { background: {{latar}}; }
.bslib-sidebar-layout > .collapse-toggle { color: {{marun}}; }

/* Kode butir */
code.kode { background: {{muda}}; color: {{marun}}; padding: 1px 7px; border-radius: 6px;
            font-weight: 700; font-size: .78rem; white-space: nowrap; }
.kode-besar { font-size: 1rem !important; padding: 3px 10px !important; background: {{emas}} !important; color: {{gelap}} !important; }
.keterangan { background: {{muda}}; border-left: 5px solid {{merah}}; border-radius: 10px;
              padding: .75rem 1rem; margin: .25rem 0 1rem 0; }
.keterangan .label { font-size: .72rem; text-transform: uppercase; letter-spacing: .08em; color: {{merah}}; font-weight: 700; }
.keterangan .fs-5 { color: {{judul}}; font-weight: 600; }

/* Dropdown per zona */
.accordion-zona .accordion-item { border: 0; margin-bottom: .45rem; border-radius: 10px !important; overflow: hidden;
                                  box-shadow: 0 1px 4px rgba(78,7,0,.15); }
.accordion-zona .accordion-button { background: #fff; font-weight: 600; font-size: .88rem; padding: .6rem .8rem; color: {{judul}}; }
.accordion-zona .accordion-button:hover { background: {{muda}}; }
.accordion-zona .accordion-button:not(.collapsed) { background: linear-gradient(120deg, {{marun2}}, {{marun}}); color: #fff; box-shadow: none; }
.accordion-zona .accordion-button:not(.collapsed)::after { filter: invert(1) brightness(2); }
.accordion-zona .accordion-body { padding: .4rem .35rem .6rem .35rem; background: #fff; max-height: 52vh; overflow-y: auto; }
.judul-zona { display: flex; flex-direction: column; gap: 2px; width: 100%; }
.hitung { display: flex; gap: 4px; }
.hitung span { font-size: .68rem; font-weight: 700; border-radius: 10px; padding: 0 7px; color: #fff; }
.sub-bagian { font-size: .7rem; font-weight: 800; text-transform: uppercase; letter-spacing: .08em;
              color: {{merah}}; margin: .5rem .4rem .15rem .4rem; }
.sub-komponen { font-size: .72rem; color: #7a6461; margin: .3rem .5rem .1rem .5rem; font-style: italic; }
a.item-butir { display: flex; gap: .45rem; align-items: flex-start; padding: .32rem .5rem; border-radius: 8px;
               color: #2b2b2b; text-decoration: none; font-size: .82rem; line-height: 1.25; transition: background .15s ease; }
a.item-butir:hover { background: {{muda}}; }
a.item-butir.aktif { background: {{marun}}; color: #fff; }
a.item-butir.aktif code.kode { background: {{emas}}; color: {{gelap}}; }
.titik { width: 10px; height: 10px; border-radius: 50%; flex: none; margin-top: 4px; box-shadow: 0 0 0 2px #fff; }

/* Tabel */
table.dataTable thead th { background: {{marun}}; color: #fff; }
table.dataTable tbody tr:hover td { background: {{muda}}; }
table.dataTable tbody tr.selected td { box-shadow: inset 0 0 0 9999px rgba(249,176,61,.30) !important; }
.kriteria td { vertical-align: top; padding: 4px 8px; }
.accordion-button:not(.collapsed) { background: {{muda}}; color: {{marun}}; }

/* Tombol */
.btn-primary { background: {{merah}}; border-color: {{merah}}; }
.btn-primary:hover { background: {{marun}}; border-color: {{marun}}; }
")

# JavaScript kecil: menandai butir aktif di daftar tanpa menggambar ulang daftar
JS_APLIKASI <- "
Shiny.addCustomMessageHandler('tandai_butir', function(kode) {
  document.querySelectorAll('a.item-butir').forEach(function(a) { a.classList.toggle('aktif', a.dataset.kode === kode); });
  var el = document.querySelector('a.item-butir[data-kode=\"' + kode + '\"]');
  if (el) setTimeout(function() { el.scrollIntoView({block: 'nearest', behavior: 'smooth'}); }, 350);
});
"

# Pertanyaan panjang -> dipotong pada batas kata; teks lengkap jadi keterangan
ringkas_teks <- function(x, n = 55) {
  x <- gsub("\\s+", " ", trimws(x))
  panjang <- nchar(x) > n
  potong <- sub("\\s+\\S*$", "", substr(x, 1, n))
  ifelse(panjang, paste0(potong, "\u2026"), x)
}

chip_kode <- function(kode, besar = FALSE) htmltools::tags$code(class = paste("kode", if (besar) "kode-besar"), kode)

# Versi HTML untuk tabel DT (kode + judul singkat, teks lengkap muncul saat disorot)
butir_html <- function(kode, pertanyaan, n = 55) {
  sprintf('<span title="%s"><code class="kode">%s</code> %s</span>',
          htmltools::htmlEscape(pertanyaan, attribute = TRUE), htmltools::htmlEscape(kode),
          htmltools::htmlEscape(ringkas_teks(pertanyaan, n)))
}

# =============================================================================
# Header ala med.unhas.ac.id:
#   ribbon merah di paling atas (menempel saat di-scroll, efek hover)
#   -> banner/gambar utama di bawahnya.
# Banner tampil penuh di Beranda dan menjadi pita tipis di menu lain agar
# tidak menghabiskan layar saat mengisi LKE.
# Logo & banner diganti dari menu Pengaturan (disimpan di www/kustom/, tidak
# ikut ke GitHub) atau dengan menaruh file logo.* / header.* di folder itu.
# =============================================================================

gambar_kustom <- function(jenis) {
  f <- list.files("www/kustom", pattern = paste0("^", jenis, "\\.(png|jpe?g|svg|webp)$"), ignore.case = TRUE)
  if (length(f)) file.path("www/kustom", f[1]) else NA_character_
}

gambar_tampilan <- function(jenis = c("logo", "header")) {
  jenis <- match.arg(jenis)
  f <- gambar_kustom(jenis)
  if (!is.na(f)) paste0("kustom/", basename(f), "?v=", as.integer(file.mtime(f)))
  else paste0("gambar/", jenis, "_bawaan.svg")
}

banner_ui <- function() {
  bawaan <- is.na(gambar_kustom("header"))
  htmltools::div(id = "banner-situs", class = if (bawaan) "pakai-bawaan",
    htmltools::span(id = "sumber-logo", `data-src` = gambar_tampilan("logo"), style = "display:none"),
    htmltools::div(class = "bingkai",
      htmltools::img(src = gambar_tampilan("header"), id = "gambar-banner", alt = "Banner"),
      htmltools::div(class = "keterangan-banner",
        htmltools::div(class = "nama-unit", KONFIG$unit),
        htmltools::div(class = "subjudul", "Dashboard Lembar Kerja Evaluasi Zona Integritas menuju WBK/WBBM"),
        htmltools::span(class = "pil-tahun", paste("Tahun penilaian", KONFIG$tahun)))))
}

kaki_ui <- function() {
  info <- c(KONFIG$alamat, KONFIG$email)
  htmltools::div(class = "kaki-situs",
    htmltools::div(htmltools::tags$b(KONFIG$unit), htmltools::br(), "Dashboard LKE Zona Integritas menuju WBK/WBBM"),
    htmltools::div(class = "kanan", lapply(info[nzchar(info)], htmltools::div)))
}

CSS_HEADER <- isi_warna("
/* --- ribbon merah: menempel di atas, seperti menu situs fakultas --- */
body.bslib-page-navbar > nav.navbar { position: sticky; top: 0; z-index: 1030; background: {{merah}} !important;
  padding-top: 0; padding-bottom: 0; min-height: 64px; transition: box-shadow .25s ease, min-height .25s ease; }
body.pita-lekat > nav.navbar { box-shadow: 0 6px 18px rgba(78,7,0,.45); min-height: 56px; }
nav.navbar .navbar-brand { color: #fff !important; font-weight: 800; display: flex; align-items: center; gap: .55rem;
                           font-size: 1.05rem; margin-right: 1.5rem; }
nav.navbar .navbar-brand img { height: 38px; transition: height .25s ease; }
body.pita-lekat nav.navbar .navbar-brand img { height: 32px; }
nav.navbar .nav-underline { gap: 0; }
nav.navbar .nav-link { position: relative; color: #fff !important; font-weight: 600; font-size: .95rem;
                       padding: 1.15rem .95rem !important; border: 0 !important;
                       transition: background .2s ease, color .2s ease; }
nav.navbar .nav-link::after { content: ''; position: absolute; left: 50%; right: 50%; bottom: 0; height: 4px;
                              background: {{emas}}; border-radius: 4px 4px 0 0; transition: left .25s ease, right .25s ease; }
nav.navbar .nav-link:hover { background: rgba(0,0,0,.16); color: {{emas}} !important; }
nav.navbar .nav-link:hover::after { left: 14%; right: 14%; }
nav.navbar .nav-link.active { color: {{emas}} !important; background: rgba(0,0,0,.22); }
nav.navbar .nav-link.active::after { left: 0; right: 0; }

/* --- banner/gambar utama --- */
#banner-situs { padding: 1rem 1rem .25rem 1rem; margin-bottom: 1rem; }
#banner-situs .bingkai { position: relative; border-radius: 12px; overflow: hidden; background: {{gelap}};
                         box-shadow: 0 10px 26px rgba(78,7,0,.30); }
#gambar-banner { display: block; width: 100%; height: auto; max-height: 560px; object-fit: cover; object-position: center 30%;
                 transition: max-height .35s ease, height .35s ease; }
#banner-situs .keterangan-banner { position: absolute; left: 0; top: 0; bottom: 0; display: none; flex-direction: column;
  justify-content: center; padding: 1.5rem 2.25rem; max-width: 70%; }
#banner-situs.pakai-bawaan .keterangan-banner { display: flex; }
#banner-situs .nama-unit { color: #fff; font-size: 2rem; font-weight: 800; line-height: 1.15; text-shadow: 0 2px 10px rgba(0,0,0,.4); }
#banner-situs .subjudul { color: {{emas}}; font-size: 1.1rem; font-weight: 700; margin-top: .35rem; }
#banner-situs .pil-tahun { align-self: flex-start; margin-top: .7rem; background: {{emas}}; color: {{gelap}}; font-weight: 800;
                           font-size: .78rem; padding: 3px 14px; border-radius: 20px; }
/* Menu selain Beranda: banner jadi pita tipis */
body.banner-ringkas #gambar-banner { height: 120px; max-height: 120px; object-position: center 22%; }
body.banner-ringkas #banner-situs .nama-unit { font-size: 1.3rem; }
body.banner-ringkas #banner-situs .subjudul, body.banner-ringkas #banner-situs .pil-tahun { display: none; }

/* --- kaki halaman --- */
.kaki-situs { margin-top: 2rem; background: {{marun}}; color: rgba(255,255,255,.88); padding: 1.25rem 1.5rem;
              display: flex; gap: 2rem; flex-wrap: wrap; border-top: 4px solid {{emas}}; font-size: .85rem; }
.kaki-situs .kanan { margin-left: auto; text-align: right; }
@media (max-width: 991px) { #banner-situs .nama-unit { font-size: 1.3rem; } #banner-situs .keterangan-banner { max-width: 100%; } }
")

JS_HEADER <- "
document.addEventListener('DOMContentLoaded', function() {
  var banner = document.getElementById('banner-situs');
  var nav = document.querySelector('body > nav.navbar');
  if (banner && nav) nav.after(banner);
  var brand = nav ? nav.querySelector('.navbar-brand') : null;
  if (brand) { var img = document.createElement('img'); img.className = 'logo-kecil'; img.alt = 'Logo';
               var sl = document.getElementById('sumber-logo'); img.src = sl ? sl.dataset.src : 'gambar/logo_bawaan.svg'; brand.prepend(img); }
  var atur = function() {
    var aktif = document.querySelector('#nav .nav-link.active');
    document.body.classList.toggle('banner-ringkas', !!aktif && aktif.dataset.value !== 'Beranda');
  };
  document.querySelectorAll('#nav .nav-link').forEach(function(a) { a.addEventListener('shown.bs.tab', atur); });
  atur();
  window.addEventListener('scroll', function() { document.body.classList.toggle('pita-lekat', window.scrollY > 10); }, {passive: true});
});
$(document).on('shown.bs.tab', '#nav .nav-link', function(e) {
  document.body.classList.toggle('banner-ringkas', e.target.dataset.value !== 'Beranda');
});
Shiny.addCustomMessageHandler('ganti_gambar', function(m) {
  if (m.jenis === 'logo') {
    document.querySelectorAll('nav.navbar .logo-kecil').forEach(function(i) { i.src = m.url; });
  } else {
    document.getElementById('gambar-banner').src = m.url;
    document.getElementById('banner-situs').classList.toggle('pakai-bawaan', m.url.indexOf('gambar/') === 0);
  }
});
"
