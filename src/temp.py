B = 'B'
V = 'V'
W = 'W'
A = 'A'

l = [[B, B, V, V, B, B, B, V, V, B, B],
    [V, V, V, W, B, B, B, W, V, V, V],
    [V, V, V, V, V, V, V, V, V, V, V],
    [V, V, V, V, V, V, V, V, V, V, V],
    [W, W, V, V, V, V, V, V, V, W, W],
    [B, B, V, V, V, A, V, V, V, B, B],
    [W, W, V, V, V, V, V, V, V, W, W],
    [V, V, V, V, V, V, V, V, V, V, V],
    [V, V, V, V, V, V, V, V, V, V, V],
    [V, V, V, W, B, B, B, W, V, V, V],
    [B, B, V, V, B, B, B, V, V, B, B]]
s = "["
for i in range(len(l)):
    for j in range(len(l)):
        s += l[j][i] + ','
print(s[:-1]+']')
