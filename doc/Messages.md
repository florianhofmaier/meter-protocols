| purpose                | msg     | dir   | format | length           | c-field         | ell                  | tpl, ci             | apl                            |
|------------------------|---------|-------|--------|------------------|-----------------|----------------------|---------------------|--------------------------------|
| link layer reset       | SND-NKE | down  | Ft1.2  | fixed length     | 40h             | -                    | -                   | -                              |
| request alarms         | REQ-UD1 | down  | Ft1.2  | fixed length     | 5Ah/7Ah         | -                    | -                   | -                              |
| request user data      | REQ-UD2 | down  | Ft1.2  | fixed length     | 5Bh/7Bh         | -                    | -                   | -                              |
| application reset      | SND-UD  | down  | Ft1.2  | variable length  | 53h/73h         | -                    | None, 50h           | -                              |
| select data            | SND-UD  | down  | Ft1.2  | variable length  | 53h/73h         | -                    | None, 51h           | command record list            |
| change primary address | SND-UD  | down  | Ft1.2  | variable length  | 53h/73h         | -                    | None, 51h           | command record list            |
| select device          | SND-UD  | down  | Ft1.2  | variable length  | 53h/73h         | -                    | None, 52h           | meter address (optional 0c/78) |
| confirm                | CNF     | up    | Ft1.2  | single character | -               | -                    | -                   | -                              |
| respond alarm          | RSP-UD  | up    | Ft1.2  | variable length  | 08h/18h/28h/38h | -                    | long, 75h           | alarm byte                     |
| respond user data      | RSP-UD  | up    | Ft1.2  | variable length  | 08h/18h/28h/38h | -                    | long, 72h           | (DIB/VIB/DATA, ...)            |
| send no response       | SND-NR  | up    | Ft3    | variable length  | 08h/18h/28h/38h | optional(short, 8Ch) | short/long, 72h/7Ah | (DIB/VIB/DATA, ...)            |
