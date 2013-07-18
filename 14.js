var res = [];

function seq (n) {
  if(n % 2 == 0)
    return n/2;
  else
    return 3*n + 1;
}

function length(i) {
  if(res[i] === undefined)
    res[i] = 1 + length(seq(i));
  return res[i];
}

res[1] = 1;
m = 1;
index = 1;
for (i = 1; i < 1000000; i++) {
  l = length(i);
  if(l>m) {
    m = l;
    index = i;
  }
}

console.log(index);