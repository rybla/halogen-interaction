function getFileType(url: string): string | null {
  const match = url.match(/\.([a-zA-Z0-9]+)(\?.*)?$/);
  return match ? match[1] : null;
}

function log(msg: string) {
  console.log(`[server] ${msg}`);
}

const server = Bun.serve({
  port: 8000,
  async fetch(req) {
    var path = new URL(req.url).pathname;
    if (path.endsWith("/")) path = `${path}index.html`;
    log(`GET ${path}`);
    const file = Bun.file(`./dist${path}`);
    if (await file.exists()) {
      const fileType = getFileType(path);
      switch (fileType) {
        case 'txt': case null: return new Response(await file.text(), { headers: { 'Content-Type': "text/plain" } });
        case 'js': return new Response(await file.text(), { headers: { 'Content-Type': "application/javascript" } });
        case 'css': return new Response(await file.text(), { headers: { 'Content-Type': "text/css" } });
        case 'html': return new Response(await file.text(), { headers: { 'Content-Type': "text/html" } });
        case 'json': return new Response(await file.text(), { headers: { 'Content-Type': "application/json" } });
        case 'jpeg': case 'png': case 'ico': return new Response(file, { headers: { 'Content-Type': `image/${fileType}` } });
        default: {
          log(`unhandled file type: ${fileType}`)
          return new Response(null, { status: 404 })
        };
      }
    } else {
      log(`unknown resource: ${path}`);
      return new Response(null, { status: 404 });
    }
  }
});

console.log(`[server] listening on ${server.url}`);
