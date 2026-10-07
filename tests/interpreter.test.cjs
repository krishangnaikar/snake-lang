const {test}=require('node:test');
const assert=require('node:assert/strict');
const {spawnSync}=require('node:child_process');
const fs=require('node:fs');
const os=require('node:os');
const path=require('node:path');
const entry=path.resolve(__dirname,'../snake.js');
function run(code) {
  const dir=fs.mkdtempSync(path.join(os.tmpdir(),'snake-tests-'));
  const file=path.join(dir,'test.snk');
  try {
    fs.writeFileSync(file,code);
    const result=spawnSync(process.execPath,[entry,file],{encoding:'utf8',timeout:5000});
    assert.ifError(result.error);
    return result;
  } finally {fs.rmSync(dir,{recursive:true,force:true});}
}
test('arithmetic honors multiplication precedence',()=>{
  const result=run('print(2 + 3 * 4)\n');
  assert.equal(result.stderr,''); assert.equal(result.stdout.trim(),'14');
});
test('functions accept arguments and return values',()=>{
  const result=run('def add(a, b) { return a + b }\nprint(add(2, 5))\n');
  assert.equal(result.stderr,''); assert.equal(result.stdout.trim(),'7');
});
test('undefined variable produces a useful diagnostic',()=>{
  const result=run('print(missing_variable)\n');
  assert.match(result.stderr,/Undefined variable/); assert.match(result.stderr,/missing_variable/);
});
test('missing filename prints usage and returns failure',()=>{
  const result=spawnSync(process.execPath,[entry],{encoding:'utf8',timeout:5000});
  assert.equal(result.status,1); assert.match(result.stderr,/Usage:/);
});
