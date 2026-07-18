import fs from "node:fs";

const files = ["www/app.js", "www/pincards.js"];
const handlerPattern = /Shiny\.addCustomMessageHandler\(\s*["'][^"']+["']\s*,\s*function\s*\(([^)]*)\)/g;
let seen = 0;
const invalid = [];

for (const file of files) {
  const source = fs.readFileSync(file, "utf8");
  for (const match of source.matchAll(handlerPattern)) {
    seen += 1;
    const params = match[1]
      .split(",")
      .map((value) => value.trim())
      .filter(Boolean);
    if (params.length !== 1) invalid.push(`${file}: ${match[0]}`);
  }
}

if (seen !== 6) {
  throw new Error(`expected 6 Shiny custom message handlers, found ${seen}`);
}
if (invalid.length) {
  throw new Error(`Shiny custom message handlers must accept exactly one payload argument:\n${invalid.join("\n")}`);
}

console.log(`OK: ${seen} Shiny custom message handlers accept exactly one payload argument.`);
