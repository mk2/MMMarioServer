MMMarioServer
===================

Multi play game works on browser supporting websocket.

Examples of Messages
===

Client to Server Messages
---

### MOV rect()
- キャラ動く

### BLK rect()
- ブロック生成

### DIE
- 死亡通知

### NAM number()
- 名前更新

### RMS
- 部屋一覧を取得

### NRM
- 新しい部屋を作る

### JRM number()
- 指定した番号の部屋に参加

Server to Client Messages
---

### REC number() rect() number() rect()
- 現在のキャラクター一覧

### BLK rect()
- ブロック一覧

### RED
- 待機状態確認

### LOS
- 負け

### WIN
- 勝ち

## Screen shots

![ss1](https://raw.githubusercontent.com/mk2/MMMarioServer/master/doc/screenshot1.jpg)

![ss2](https://raw.githubusercontent.com/mk2/MMMarioServer/master/doc/screenshot2.jpg)