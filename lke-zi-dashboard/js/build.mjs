// Membangun www/vendor/zi-ui.min.js dan daftar lisensi pustaka pihak ketiga.
// Jalankan dari folder js/:  npm install && npm run build
import { build } from 'esbuild';
import { readFileSync, writeFileSync, existsSync, mkdirSync } from 'node:fs';
import { join } from 'node:path';

const keluaran = '../www/vendor';
mkdirSync(keluaran, { recursive: true });

const hasil = await build({
  entryPoints: ['src/main.jsx'],
  bundle: true,
  minify: true,
  format: 'iife',
  target: ['es2019'],
  jsx: 'automatic',
  define: { 'process.env.NODE_ENV': '"production"' },
  legalComments: 'none',
  metafile: true,
  outfile: join(keluaran, 'zi-ui.min.js'),
});

// Kumpulkan lisensi setiap paket yang ikut dibundel
const paket = new Set();
for (const masukan of Object.keys(hasil.metafile.inputs)) {
  const m = masukan.match(/node_modules\/((?:@[^/]+\/)?[^/]+)/);
  if (m) paket.add(m[1]);
}
let teks = 'Pustaka pihak ketiga di dalam zi-ui.min.js\n==========================================\n';
for (const p of [...paket].sort()) {
  const dir = join('node_modules', p);
  const pj = JSON.parse(readFileSync(join(dir, 'package.json'), 'utf8'));
  const berkas = ['LICENSE', 'LICENSE.md', 'LICENSE.txt', 'license', 'LICENCE'].map((f) => join(dir, f)).find(existsSync);
  teks += `\n--- ${p}@${pj.version} (${pj.license}) ---\n${berkas ? readFileSync(berkas, 'utf8').trim() : '(berkas lisensi tidak disertakan dalam paket)'}\n`;
}
writeFileSync(join(keluaran, 'LISENSI-PIHAK-KETIGA.txt'), teks);
console.log(`ok: ${[...paket].length} paket dibundel`);
