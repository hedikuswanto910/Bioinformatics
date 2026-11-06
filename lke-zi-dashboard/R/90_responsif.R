# =============================================================================
# 90_responsif.R - Tata letak responsif (mobile-first)
# -----------------------------------------------------------------------------
# Breakpoint:  mobile < 768 px  |  tablet 768-1023 px  |  desktop >= 1024 px
#
# Komponen:
#   kisi()          : grid kolom (mobile 1 kolom, tablet & desktop diatur)
#   kisi_kpi()      : kartu ringkasan 1 / 2 / 4 kolom
#   tata_samping()  : panel samping + isi utama
#                     desktop  = panel tampil penuh di kiri
#                     tablet   = panel bisa diciutkan (tombol panah)
#                     mobile   = "laci": disembunyikan, dibuka tombol hamburger
#                                "tumpuk": panel ditumpuk di atas isi
#   label_menu()    : label menu panjang + versi pendek (desktop sempit)
# Wadah utama memakai %, rem, fr, dan minmax(); tidak ada lebar px tetap.
# =============================================================================

.kolom_fr <- function(x) {
  if (is.character(x)) return(x)
  paste(sprintf("minmax(0, %sfr)", x), collapse = " ")
}

# Grid umum. desktop/tablet: vektor bobot kolom (mis. c(7, 5)) atau teks CSS.
kisi <- function(..., desktop = c(1, 1), tablet = 1, kelas = NULL) {
  htmltools::div(class = paste("kisi", kelas),
                 style = sprintf("--kolom-d: %s; --kolom-t: %s;", .kolom_fr(desktop), .kolom_fr(tablet)), ...)
}

kisi_kpi <- function(...) htmltools::div(class = "kisi-kpi", ...)

# Satu kartu KPI rata tengah: ikon kecil, nilai (output Shiny), keterangan
kartu_kpi <- function(judul, id_output, ikon) {
  htmltools::div(class = "kpi-kartu", role = "group", `aria-label` = judul,
    htmltools::span(class = "kpi-ikon", `aria-hidden` = "true", shiny::icon(ikon)),
    htmltools::div(class = "kpi-nilai", shiny::textOutput(id_output, inline = TRUE)),
    htmltools::div(class = "kpi-judul", judul))
}

tata_samping <- function(id, samping, ..., judul = "Menu", mobile = c("laci", "tumpuk")) {
  mobile <- match.arg(mobile)
  htmltools::div(id = id, class = paste0("tata-samping mobile-", mobile),
    if (mobile == "laci") htmltools::tags$button(
      type = "button", class = "btn tombol-laci", `aria-controls` = paste0(id, "-panel"), `aria-expanded` = "false",
      onclick = sprintf("zi_laci('%s', true)", id), shiny::icon("bars"), htmltools::span(judul)),
    if (mobile == "laci") htmltools::div(class = "latar-laci", onclick = sprintf("zi_laci('%s', false)", id)),
    htmltools::tags$aside(id = paste0(id, "-panel"), class = "panel-samping", `aria-label` = judul,
      htmltools::div(class = "kepala-samping",
        htmltools::span(class = "judul-samping", judul),
        htmltools::tags$button(type = "button", class = "btn tombol-ciut", title = "Ciutkan/lebarkan panel",
                               `aria-label` = "Ciutkan panel", onclick = sprintf("zi_ciut('%s')", id), shiny::icon("angles-left")),
        if (mobile == "laci") htmltools::tags$button(type = "button", class = "btn tombol-tutup", `aria-label` = "Tutup",
                                                     onclick = sprintf("zi_laci('%s', false)", id), shiny::icon("xmark"))),
      htmltools::div(class = "isi-samping", samping)),
    htmltools::div(class = "isi-utama", ...))
}

label_menu <- function(panjang, pendek) {
  htmltools::tagList(htmltools::span(class = "label-panjang", panjang), htmltools::span(class = "label-pendek", pendek))
}

CSS_RESPONSIF <- isi_warna("
/* ---------- dasar mobile-first ---------- */
*, *::before, *::after { box-sizing: border-box; }
html { scroll-padding-top: calc(var(--tinggi-nav, 4rem) + 0.5rem); }
body { overflow-x: hidden; }
img, svg, video { max-width: 100%; }

/* Wadah konten: maksimum 1440 px di tengah, sisi kosong di layar lebih lebar */
body.bslib-page-navbar > .container-fluid,
nav.navbar > .container-fluid,
#banner-situs, .kaki-situs > .wadah-kaki {
  width: 100%; max-width: 90rem; margin-inline: auto;
}
body.bslib-page-navbar > .container-fluid { padding-inline: clamp(0.75rem, 2.5vw, 1.5rem); padding-block: 1rem; }
#banner-situs { padding: 1rem clamp(0.75rem, 2.5vw, 1.5rem) 0; margin-bottom: 0; }

/* ---------- navbar: sticky, tidak menutupi konten (tetap di alur dokumen) ---------- */
body.bslib-page-navbar > nav.navbar { position: sticky; top: 0; }
nav.navbar > .container-fluid { display: flex; flex-wrap: wrap; align-items: center; padding-inline: clamp(0.75rem, 2.5vw, 1.5rem); }
nav.navbar .navbar-header { display: flex; align-items: center; justify-content: space-between; width: 100%; }
nav.navbar .navbar-toggle { display: inline-flex; align-items: center; justify-content: center; min-width: 2.75rem; min-height: 2.75rem;
  margin: 0; border: 1px solid rgba(255,255,255,.5); border-radius: .5rem; background: transparent; }
nav.navbar .navbar-toggle { flex-direction: column; gap: .3rem; padding: 0 .65rem; }
nav.navbar .navbar-toggle .icon-bar { display: block; flex: none; width: 1.4rem; height: 2px; margin: 0 !important;
                                      background: #fff; border-radius: 1px; }
nav.navbar .navbar-collapse { width: 100%; }
nav.navbar .navbar-collapse.show { max-height: calc(100vh - var(--tinggi-nav, 4rem)); overflow-y: auto; }
nav.navbar .navbar-nav { flex-direction: column; width: 100%; padding-bottom: .5rem; }
nav.navbar .nav-link { padding: .75rem 1rem !important; min-height: 2.75rem; display: flex; align-items: center; }
nav.navbar .nav-link::after { left: 0 !important; right: auto !important; top: 0; bottom: 0 !important; width: 0; height: auto !important;
                              border-radius: 0 4px 4px 0; transition: width .2s ease !important; }
nav.navbar .nav-link:hover::after { width: 4px; }
nav.navbar .nav-link.active::after { width: 4px; }
.label-pendek { display: none; }
nav.navbar .navbar-brand { font-size: 1rem; }

/* ---------- KPI 1 kolom ---------- */
.kisi-kpi { display: grid; gap: 1rem; grid-template-columns: minmax(0, 1fr); margin-bottom: 1rem; }
.kisi-kpi > .kpi-kartu { margin: 0; }
.bslib-value-box .value-box-value { font-size: clamp(1.4rem, 1.1rem + 1.2vw, 2rem); line-height: 1.15; }

/* ---------- grid umum 1 kolom ---------- */
.kisi { display: grid; gap: 1rem; grid-template-columns: minmax(0, 1fr); margin-bottom: 1rem; align-items: start; }
.kisi > * { min-width: 0; margin-bottom: 0 !important; }

/* ---------- panel samping: mobile ---------- */
.tata-samping { display: grid; grid-template-columns: minmax(0, 1fr); gap: 1rem; margin-bottom: 1rem; }
.tata-samping > * { min-width: 0; }
.panel-samping { background: {{samping}}; border-radius: .75rem; box-shadow: 0 6px 18px rgba(78,7,0,.14); }
.kepala-samping { display: flex; align-items: center; gap: .5rem; padding: .5rem .75rem; border-bottom: 1px solid #f0dcd8; }
.judul-samping { font-weight: 700; color: {{marun}}; margin-right: auto; }
.isi-samping { padding: .75rem; }
.tombol-ciut { display: none; }
.tombol-laci, .tombol-tutup { display: none; }
.latar-laci { display: none; }
/* mode laci (hanya mobile) */
.mobile-laci .tombol-laci { display: inline-flex; align-items: center; gap: .5rem; justify-self: start;
  min-height: 2.75rem; padding-inline: 1rem; background: {{marun}}; color: #fff; border-radius: .6rem; font-weight: 600; }
.mobile-laci .tombol-tutup { display: inline-flex; align-items: center; justify-content: center; min-width: 2.75rem; min-height: 2.75rem; }
.mobile-laci .panel-samping { position: fixed; top: 0; bottom: 0; left: 0; width: min(88vw, 24rem); z-index: 1060;
  border-radius: 0 .75rem .75rem 0; overflow-y: auto; transform: translateX(-105%); transition: transform .25s ease;
  visibility: hidden; }
.mobile-laci.buka .panel-samping { transform: none; visibility: visible; }
.mobile-laci.buka .latar-laci { display: block; position: fixed; inset: 0; background: rgba(30,0,0,.5); z-index: 1055; }
.mobile-laci .accordion-zona .accordion-body { max-height: none; }

/* ---------- tabel & konten lebar: geser horizontal, tidak memecah layout ---------- */
.card, .card-body, .isi-utama, .kisi > * { min-width: 0; }
.dataTables_wrapper { width: 100%; }
.dataTables_scrollBody, .gulir-x { overflow-x: auto; -webkit-overflow-scrolling: touch; }
.dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter { float: none; text-align: left; }
.dataTables_wrapper .dataTables_filter input { width: 100%; margin-left: 0; }
.dataTables_wrapper .dataTables_length label, .dataTables_wrapper .dataTables_filter label {
  display: flex; align-items: center; gap: .5rem; flex-wrap: wrap; width: 100%; }
.dataTables_wrapper .dataTables_length select { width: auto; }
/* di layar sempit kolom diberi lebar minimum; tabel digeser, bukan diremas */
table.dataTable th, table.dataTable td { min-width: 6.5rem; }
table.dataTable td:has(code.kode) { min-width: 14rem; }
table.dataTable { width: 100% !important; }
.card-header { flex-wrap: wrap; gap: .5rem; }
.kepala-butir { display: flex; flex-wrap: wrap; gap: .5rem; align-items: center; justify-content: space-between; }
.kepala-butir .navigasi-butir { display: flex; gap: .5rem; flex-wrap: wrap; }
.shiny-input-container { width: 100% !important; max-width: 100%; }
#banner-situs .bingkai { border-radius: .75rem; }
body.banner-ringkas #gambar-banner { height: 4.5rem; max-height: 4.5rem; }
#gambar-banner { max-height: min(35rem, 60vh); }
.kaki-situs { display: block; padding: 0; }
.kaki-situs > .wadah-kaki { display: flex; flex-wrap: wrap; gap: 1rem 2rem; padding: 1.25rem clamp(0.75rem, 2.5vw, 1.5rem); }
.kaki-situs .kanan { margin-left: 0; text-align: left; }

/* ---------- layar sentuh: target minimal 44 x 44 px ---------- */
@media (pointer: coarse) {
  .btn, .nav-link, a.item-butir, .accordion-button, .navbar-toggle, .selectize-input,
  .form-control, .form-select, .paginate_button, .dataTables_length select, .dataTables_filter input,
  .tombol-ciut, .tombol-tutup, .tombol-laci, .action-button, .btn-file {
    min-height: 2.75rem; }
  .btn, .navbar-toggle, .paginate_button, .tombol-ciut, .tombol-tutup { min-width: 2.75rem; }
  a.item-butir { align-items: center; padding-block: .5rem; }
  .form-check-input { width: 1.4rem; height: 1.4rem; margin-top: .1rem; }
  .form-check, .radio label, .checkbox label, .radio-inline, .checkbox-inline { min-height: 2.75rem; display: inline-flex; align-items: center; gap: .4rem; }
  .shiny-options-group .radio-inline, .shiny-options-group .form-check-inline { padding-right: .75rem; }
  .dataTables_wrapper .paginate_button { display: inline-flex !important; align-items: center; justify-content: center; }
}

/* ---------- tablet: 768 - 1023 px ---------- */
@media (min-width: 768px) {
  .kisi-kpi { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .kisi { grid-template-columns: var(--kolom-t, minmax(0, 1fr)); }
  .tata-samping { grid-template-columns: minmax(15rem, 34%) minmax(0, 1fr); align-items: start; }
  .tata-samping.ciut { grid-template-columns: 3.25rem minmax(0, 1fr); }
  .tata-samping.ciut .isi-samping, .tata-samping.ciut .judul-samping { display: none; }
  .tata-samping.ciut .tombol-ciut > * { transform: rotate(180deg); }
  .tombol-ciut { display: inline-flex; align-items: center; justify-content: center; flex: none; padding: 0;
                 width: 2.75rem; height: 2.75rem; color: {{marun}}; border: 1px solid #ecd2cd; background: #fff; }
  .tata-samping.ciut .kepala-samping { padding-inline: .25rem; justify-content: center; }
  .panel-samping { position: sticky; top: calc(var(--tinggi-nav, 4rem) + .75rem);
                   max-height: calc(100vh - var(--tinggi-nav, 4rem) - 1.5rem); overflow-y: auto; }
  /* laci kembali menjadi panel biasa */
  .mobile-laci .panel-samping { position: sticky; width: auto; transform: none; visibility: visible; border-radius: .75rem; z-index: 1; }
  .mobile-laci .tombol-laci, .mobile-laci .tombol-tutup, .mobile-laci .latar-laci { display: none !important; }
  .mobile-laci .accordion-zona .accordion-body { max-height: 52vh; }
  body.banner-ringkas #gambar-banner { height: 6.5rem; max-height: 6.5rem; }
  .dataTables_wrapper .dataTables_length { float: left; }
  .dataTables_wrapper .dataTables_filter { float: right; text-align: right; }
  .dataTables_wrapper .dataTables_filter input { width: auto; margin-left: .5rem; }
  .dataTables_wrapper .dataTables_length label, .dataTables_wrapper .dataTables_filter label { width: auto; display: inline-flex; }
  table.dataTable th, table.dataTable td { min-width: 0; }
  table.dataTable td:has(code.kode) { min-width: 12rem; }
  .kaki-situs .kanan { margin-left: auto; text-align: right; }
}

/* ---------- desktop: >= 1024 px ---------- */
@media (min-width: 1024px) {
  .kisi-kpi { grid-template-columns: repeat(4, minmax(0, 1fr)); }
  .kisi { grid-template-columns: var(--kolom-d, minmax(0, 1fr)); }
  .tata-samping, .tata-samping.ciut { grid-template-columns: minmax(17rem, 28%) minmax(0, 1fr); }
  .tata-samping .tombol-ciut { display: none; }
  .tata-samping.ciut .isi-samping, .tata-samping.ciut .judul-samping { display: block; }
  /* navbar horizontal satu baris */
  nav.navbar > .container-fluid { flex-wrap: nowrap; }
  nav.navbar .navbar-header { width: auto; }
  nav.navbar .navbar-toggle { display: none !important; }
  nav.navbar .navbar-collapse { display: flex !important; width: auto; flex: 1 1 auto; min-width: 0; }
  nav.navbar .navbar-nav { flex-direction: row; flex-wrap: nowrap; padding-bottom: 0; overflow-x: auto; scrollbar-width: none; }
  nav.navbar .navbar-nav::-webkit-scrollbar { display: none; }
  nav.navbar .nav-link { padding: 1.15rem .7rem !important; white-space: nowrap; font-size: .9rem; }
  nav.navbar .nav-link::after { top: auto !important; bottom: 0 !important; left: 50% !important; right: 50% !important;
    width: auto !important; height: 4px !important; border-radius: 4px 4px 0 0; transition: left .25s ease, right .25s ease !important; }
  nav.navbar .nav-link:hover::after { left: 14% !important; right: 14% !important; }
  nav.navbar .nav-link.active::after { left: 0 !important; right: 0 !important; }
  .label-panjang { display: none; } .label-pendek { display: inline; }
  nav.navbar .navbar-brand .teks-brand { display: none; }
  body.banner-ringkas #gambar-banner { height: 7.5rem; max-height: 7.5rem; }
}
/* desktop lebar: label menu lengkap */
@media (min-width: 1280px) {
  .label-panjang { display: inline; } .label-pendek { display: none; }
  nav.navbar .navbar-brand .teks-brand { display: inline; }
  nav.navbar .nav-link { padding-inline: .9rem !important; font-size: .95rem; }
}
")

JS_RESPONSIF <- "
function zi_laci(id, buka) {
  var el = document.getElementById(id); if (!el) return;
  el.classList.toggle('buka', buka);
  var t = el.querySelector('.tombol-laci'); if (t) t.setAttribute('aria-expanded', buka ? 'true' : 'false');
  document.body.style.overflow = buka ? 'hidden' : '';
}
function zi_ciut(id) { var el = document.getElementById(id); if (el) el.classList.toggle('ciut'); }
(function() {
  var ukurNav = function() {
    var nav = document.querySelector('body > nav.navbar');
    if (nav) document.documentElement.style.setProperty('--tinggi-nav', nav.offsetHeight + 'px');
  };
  document.addEventListener('DOMContentLoaded', function() {
    ukurNav();
    var nav = document.querySelector('body > nav.navbar');
    if (nav && 'ResizeObserver' in window) new ResizeObserver(ukurNav).observe(nav);
    // bungkus isi kaki halaman agar ikut lebar maksimum
    document.querySelectorAll('.kaki-situs').forEach(function(k) {
      if (!k.querySelector('.wadah-kaki')) { var w = document.createElement('div'); w.className = 'wadah-kaki';
        while (k.firstChild) w.appendChild(k.firstChild); k.appendChild(w); }
    });
    // teks brand bisa disembunyikan di desktop sempit
    var brand = document.querySelector('nav.navbar .navbar-brand');
    if (brand) brand.childNodes.forEach(function(n) {
      if (n.nodeType === 3 && n.textContent.trim()) { var s = document.createElement('span'); s.className = 'teks-brand';
        s.textContent = n.textContent.trim(); brand.replaceChild(s, n); } });
  });
  // tutup menu & laci setelah memilih (mobile/tablet)
  document.addEventListener('click', function(e) {
    var link = e.target.closest('#nav .nav-link');
    if (link && window.innerWidth < 1024) {
      var c = document.querySelector('nav.navbar .navbar-collapse.show');
      if (c && window.bootstrap) bootstrap.Collapse.getOrCreateInstance(c).hide();
    }
    var item = e.target.closest('a.item-butir');
    if (item && window.innerWidth < 768) {
      var lay = item.closest('.tata-samping'); if (lay) zi_laci(lay.id, false);
      window.scrollTo({top: 0, behavior: 'smooth'});
    }
  });
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') document.querySelectorAll('.tata-samping.buka').forEach(function(l) { zi_laci(l.id, false); });
  });
})();
"
