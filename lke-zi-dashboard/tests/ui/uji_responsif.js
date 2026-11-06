const { chromium } = require('playwright');
// Pemakaian: jalankan aplikasi di port 3838, lalu
//   node tests/ui/uji_responsif.js [folder_tangkapan_layar]
// Butuh Node.js + paket playwright (npm i playwright; npx playwright install chromium).
const SP = process.argv[2] || 'hasil_uji_responsif';
const URL = process.env.ZI_URL || URL;
require('fs').mkdirSync(SP + '/shots', { recursive: true });
const LEBAR = [
  { w: 375,  h: 812,  sentuh: true,  jenis: 'mobile' },
  { w: 768,  h: 1024, sentuh: true,  jenis: 'tablet' },
  { w: 1024, h: 768,  sentuh: false, jenis: 'desktop' },
  { w: 1440, h: 900,  sentuh: false, jenis: 'desktop' },
  { w: 1920, h: 1080, sentuh: false, jenis: 'desktop' },
];
const hasil = [];
const cek = (w, nama, ok, info = '') => { hasil.push({ w, nama, ok, info }); };

(async () => {
  const browser = await chromium.launch();
  for (const L of LEBAR) {
    const ctx = await browser.newContext({ viewport: { width: L.w, height: L.h }, hasTouch: L.sentuh, isMobile: L.sentuh, deviceScaleFactor: 2 });
    const p = await ctx.newPage();
    const errs = []; p.on('pageerror', e => errs.push(String(e)));
    const idle = async () => { await p.waitForFunction(() => window.Shiny && !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 }); await p.waitForTimeout(700); };
    await p.goto(URL); await p.waitForSelector('#plot_komponen img', { timeout: 60000 }); await idle();

    const kasar = await p.evaluate(() => matchMedia('(pointer: coarse)').matches);
    const dasar = await p.evaluate(() => {
      const nav = document.querySelector('body > nav.navbar'), c = document.querySelector('body > .container-fluid');
      const r = c.getBoundingClientRect(), tog = document.querySelector('nav .navbar-toggle');
      const kpi = getComputedStyle(document.querySelector('.kisi-kpi')).gridTemplateColumns.split(' ').length;
      const banner = document.getElementById('banner-situs').getBoundingClientRect();
      return { overflow: document.documentElement.scrollWidth - innerWidth, lebar: r.width, kiri: r.left, kanan: innerWidth - r.right,
               toggle: getComputedStyle(tog).display !== 'none', navH: nav.offsetHeight, navBawah: nav.getBoundingClientRect().bottom,
               bannerAtas: banner.top, kpi };
    });
    cek(L.w, 'tidak ada scroll horizontal halaman', dasar.overflow <= 0, `lebih ${dasar.overflow}px`);
    cek(L.w, 'lebar konten <= 1440 & di tengah', dasar.lebar <= 1440.5 && Math.abs(dasar.kiri - dasar.kanan) < 2, `lebar ${dasar.lebar}, kiri ${dasar.kiri}, kanan ${dasar.kanan}`);
    cek(L.w, 'hamburger navbar sesuai breakpoint', dasar.toggle === (L.w < 1024), `toggle ${dasar.toggle}`);
    cek(L.w, 'navbar 1 baris (desktop) / ringkas', dasar.navH <= 80, `tinggi ${dasar.navH}px`);
    cek(L.w, 'navbar tidak menutupi konten', dasar.bannerAtas >= dasar.navBawah - 0.5, `nav bawah ${dasar.navBawah}, konten ${dasar.bannerAtas}`);
    const kpiHarap = L.w < 768 ? 1 : (L.w < 1024 ? 2 : 4);
    cek(L.w, `KPI ${kpiHarap} kolom`, dasar.kpi === kpiHarap, `${dasar.kpi} kolom`);
    const grafik = await p.evaluate(() => { const i = document.querySelector('#plot_komponen img'), c = document.getElementById('plot_komponen');
      return { img: i.getBoundingClientRect().width, wadah: c.getBoundingClientRect().width }; });
    cek(L.w, 'grafik selebar wadah', Math.abs(grafik.img - grafik.wadah) < 3, `img ${grafik.img} vs wadah ${grafik.wadah}`);
    await p.screenshot({ path: `${SP}/shots/r_${L.w}_beranda.png` });

    // sticky setelah scroll
    await p.evaluate(() => window.scrollTo(0, 900)); await p.waitForTimeout(400);
    const navTop = await p.evaluate(() => document.querySelector('body > nav.navbar').getBoundingClientRect().top);
    cek(L.w, 'navbar sticky (top 0 saat scroll)', Math.abs(navTop) < 1, `top ${navTop}`);
    await p.evaluate(() => window.scrollTo(0, 0));

    // buka Isi LKE (lewat hamburger di <1024)
    if (L.w < 1024) {
      await p.click('nav .navbar-toggle'); await p.waitForTimeout(500);
      const menuTampil = await p.isVisible('a.nav-link[data-value="Isi LKE"]');
      cek(L.w, 'menu hamburger terbuka', menuTampil);
    }
    await p.click('a.nav-link[data-value="Isi LKE"]'); await p.waitForSelector('#ui_form code.kode'); await idle();
    if (L.w < 1024) {
      const tertutup = await p.evaluate(() => !document.querySelector('nav .navbar-collapse').classList.contains('show'));
      cek(L.w, 'menu menutup setelah dipilih', tertutup);
    }
    const ts = await p.evaluate(() => { const pan = document.querySelector('#ts_isi .panel-samping').getBoundingClientRect();
      const laci = document.querySelector('#ts_isi .tombol-laci'), ciut = document.querySelector('#ts_isi .tombol-ciut');
      return { kanan: pan.right, lebar: pan.width, laci: laci && getComputedStyle(laci).display !== 'none', ciut: getComputedStyle(ciut).display !== 'none' }; });
    if (L.jenis === 'mobile') {
      cek(L.w, 'sidebar tersembunyi + tombol hamburger', ts.kanan <= 0 && ts.laci, JSON.stringify(ts));
      await p.screenshot({ path: `${SP}/shots/r_${L.w}_isi.png` });
      await p.click('#ts_isi .tombol-laci'); await p.waitForTimeout(500);
      const buka = await p.evaluate(() => document.querySelector('#ts_isi .panel-samping').getBoundingClientRect().left >= -1);
      cek(L.w, 'hamburger membuka sidebar (laci)', buka);
      await p.screenshot({ path: `${SP}/shots/r_${L.w}_laci.png` });
      await p.click('.accordion-zona .accordion-button:has-text("Zona 2")'); await p.waitForTimeout(600);
      await p.click('a.item-butir[data-kode="P2.i.b"]'); await idle();
      const st = await p.evaluate(() => ({ tutup: !document.getElementById('ts_isi').classList.contains('buka'), kode: document.querySelector('#ui_form code.kode').textContent }));
      cek(L.w, 'pilih butir menutup laci & membuka butir', st.tutup && st.kode === 'P2.i.b', JSON.stringify(st));
    } else if (L.jenis === 'tablet') {
      cek(L.w, 'sidebar tampil + tombol ciut', ts.kanan > 0 && ts.ciut && !ts.laci, JSON.stringify(ts));
      await p.screenshot({ path: `${SP}/shots/r_${L.w}_isi.png` });
      await p.click('#ts_isi .tombol-ciut'); await p.waitForTimeout(500);
      const w2 = await p.evaluate(() => document.querySelector('#ts_isi .panel-samping').getBoundingClientRect().width);
      cek(L.w, 'sidebar bisa diciutkan', w2 < 70, `lebar setelah ciut ${w2}px`);
      await p.screenshot({ path: `${SP}/shots/r_${L.w}_ciut.png` });
      await p.click('#ts_isi .tombol-ciut'); await p.waitForTimeout(400);
    } else {
      cek(L.w, 'sidebar tampil penuh di kiri (tanpa ciut/laci)', ts.kanan > 0 && !ts.ciut && !ts.laci && ts.lebar > 250, JSON.stringify(ts));
      await p.screenshot({ path: `${SP}/shots/r_${L.w}_isi.png` });
    }
    const ovIsi = await p.evaluate(() => document.documentElement.scrollWidth - innerWidth);
    cek(L.w, 'Isi LKE tanpa scroll horizontal', ovIsi <= 0, `lebih ${ovIsi}px`);

    // target sentuh
    if (L.sentuh) {
      cek(L.w, 'emulasi layar sentuh (pointer: coarse)', kasar);
      const kecil = await p.evaluate(() => {
        const sel = 'button, .btn, a.nav-link, a.item-butir, .accordion-button, .selectize-input, .form-check, .radio-inline, input.form-control, textarea';
        return [...document.querySelectorAll(sel)].filter(e => { const r = e.getBoundingClientRect(); const s = getComputedStyle(e);
          return r.width > 0 && r.height > 0 && s.visibility !== 'hidden' && (r.height < 43.5 || r.width < 43.5); })
          .map(e => `${e.tagName.toLowerCase()}.${(e.className || '').toString().split(' ')[0]}:${Math.round(e.getBoundingClientRect().width)}x${Math.round(e.getBoundingClientRect().height)}`);
      });
      cek(L.w, 'target sentuh >= 44x44 (Isi LKE)', kecil.length === 0, kecil.slice(0, 6).join(', '));
    }

    // Validasi: tabel lebar
    if (L.w < 1024) { await p.click('nav .navbar-toggle'); await p.waitForTimeout(400); }
    await p.click('a.nav-link[data-value="Validasi"]'); await p.waitForSelector('#tbl_validasi table'); await idle();
    const tb = await p.evaluate(() => { const b = document.querySelector('#tbl_validasi .dataTables_scrollBody');
      return { ov: document.documentElement.scrollWidth - innerWidth, geser: b ? b.scrollWidth > b.clientWidth : null, wadah: b ? getComputedStyle(b).overflowX : null }; });
    cek(L.w, 'tabel: halaman tidak pecah', tb.ov <= 0, `lebih ${tb.ov}px`);
    if (L.w < 768) cek(L.w, 'tabel bisa digeser horizontal', tb.geser === true && /auto|scroll/.test(tb.wadah), JSON.stringify(tb));
    await p.screenshot({ path: `${SP}/shots/r_${L.w}_validasi.png` });
    if (L.sentuh) {
      const kecil = await p.evaluate(() => [...document.querySelectorAll('.paginate_button, .dataTables_length select, .dataTables_filter input')]
        .filter(e => { const r = e.getBoundingClientRect(); return r.width > 0 && (r.height < 43.5 || r.width < 43.5); })
        .map(e => `${e.className}:${Math.round(e.getBoundingClientRect().width)}x${Math.round(e.getBoundingClientRect().height)}`));
      cek(L.w, 'target sentuh >= 44x44 (tabel)', kecil.length === 0, kecil.slice(0, 5).join(', '));
    }
    cek(L.w, 'tanpa error JavaScript', errs.length === 0, errs.join(' | '));
    await ctx.close();
  }
  // grafik saat ukuran layar diubah
  const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 } }); const p = await ctx.newPage();
  await p.goto(URL); await p.waitForSelector('#plot_komponen img');
  await p.setViewportSize({ width: 600, height: 900 }); await p.waitForTimeout(3500);
  const g = await p.evaluate(() => ({ img: document.querySelector('#plot_komponen img').naturalWidth / (window.devicePixelRatio || 1),
                                      tampil: document.querySelector('#plot_komponen img').getBoundingClientRect().width,
                                      wadah: document.getElementById('plot_komponen').getBoundingClientRect().width }));
  cek('1440->600', 'grafik digambar ulang sesuai lebar baru', Math.abs(g.img - g.wadah) < 4 && Math.abs(g.tampil - g.wadah) < 3, JSON.stringify(g));
  await browser.close();
  for (const h of hasil) console.log(`${h.ok ? 'LULUS' : 'GAGAL'} | ${String(h.w).padEnd(8)} | ${h.nama}${h.ok ? '' : ' -> ' + h.info}`);
  console.log(`\nTotal: ${hasil.filter(h => h.ok).length}/${hasil.length} lulus`);
})();
