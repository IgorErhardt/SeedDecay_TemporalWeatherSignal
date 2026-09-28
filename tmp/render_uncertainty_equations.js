"use strict";

const path = require("path");
const fs = require("fs");
const sharp = require("sharp");
const { mathjax } = require("mathjax-full/js/mathjax.js");
const { TeX } = require("mathjax-full/js/input/tex.js");
const { SVG } = require("mathjax-full/js/output/svg.js");
const { liteAdaptor } = require("mathjax-full/js/adaptors/liteAdaptor.js");
const { RegisterHTMLHandler } = require("mathjax-full/js/handlers/html.js");
const { AllPackages } = require("mathjax-full/js/input/tex/AllPackages.js");

const outDir = path.join(__dirname, "uncertainty_equation_assets");
fs.mkdirSync(outDir, { recursive: true });

const adaptor = liteAdaptor();
RegisterHTMLHandler(adaptor);
const tex = new TeX({ packages: AllPackages });
const svgOutput = new SVG({ fontCache: "local" });
const doc = mathjax.document("", { InputJax: tex, OutputJax: svgOutput });

function equationSvg(latex) {
  const html = adaptor.outerHTML(doc.convert(latex, { display: true }));
  const start = html.indexOf("<svg");
  const end = html.indexOf("</svg>");
  let svg = html.slice(start, end + 6).replace(/<\?xml[^>]*>/g, "");
  if (!/xmlns="http:\/\/www\.w3\.org\/2000\/svg"/.test(svg)) {
    svg = svg.replace(/<svg /, '<svg xmlns="http://www.w3.org/2000/svg" ');
  }
  return svg.replace(/currentColor/g, "#000000");
}

const equations = [
  ["eq01_bootstrap_se", String.raw`SE_{\mathrm{boot},p}(t)=SD_b\!\left\{\widehat{\beta}^{*(b)}_p(t)\right\}`],
  ["eq02_max_deviation", String.raw`M_{pb}=\max_{t\in\mathcal{T}}\left|\frac{\widehat{\beta}^{*(b)}_p(t)-\widehat{\beta}_p(t)}{SE_{\mathrm{boot},p}(t)}\right|`],
  ["eq03_critical_value", String.raw`c^{\max}_{p,0.95}=Q_{0.95}\!\left(M_{p1},\ldots,M_{pB}\right)`],
  ["eq04_ribbon", String.raw`\widehat{\beta}_p(t)\ \pm\ c^{\max}_{p,0.95}\,SE_{\mathrm{boot},p}(t)`],
  ["eq05_integrated", String.raw`C_{pk}=\sum_{t\in B_k}\widehat{\beta}_p(t)`],
  ["eq06_boot_integrated", String.raw`C^{*(b)}_{pk}=\sum_{t\in B_k}\widehat{\beta}^{*(b)}_p(t)`],
  ["eq07_percentile_ci", String.raw`\left[Q_{0.025}\!\left(C^{*(1)}_{pk},\ldots,C^{*(B)}_{pk}\right),\ Q_{0.975}\!\left(C^{*(1)}_{pk},\ldots,C^{*(B)}_{pk}\right)\right]`]
];

(async () => {
  for (const [name, latex] of equations) {
    const svg = Buffer.from(equationSvg(latex));
    await sharp(svg, { density: 300 })
      .trim({ background: { r: 255, g: 255, b: 255, alpha: 0 } })
      .extend({ top: 14, bottom: 14, left: 18, right: 18, background: { r: 255, g: 255, b: 255, alpha: 0 } })
      .png()
      .toFile(path.join(outDir, `${name}.png`));
  }
})().catch((error) => {
  console.error(error);
  process.exit(1);
});
