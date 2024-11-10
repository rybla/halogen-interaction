import * as readline from 'readline/promises';

export const console_prompt_ = (msg) => async () => {
  const i = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  const res = await i.question(msg);
  i.close()
  return res;
}

export const console_print_ = (msg) => async () => {
  console.log(msg)
}
