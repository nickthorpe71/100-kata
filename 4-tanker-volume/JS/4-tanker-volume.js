function tankVol(h, d, vt) {
  const r = d / 2;
  const theta = 2 * Math.acos(1 - 2 * h / d);
  const liquidSegmentArea = (r ** 2 / 2) * (theta - Math.sin(theta));
  const length = vt / (Math.PI * r ** 2);
  const liquidVolume = liquidSegmentArea * length;
  return Math.floor(liquidVolume);
}

function main() {
  console.time('Execution Time');
  console.log(tankVol(40,120,3500));
  console.timeEnd('Execution Time');
}

main();

