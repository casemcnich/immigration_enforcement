```mermaid
sequenceDiagram
    TRAC total arrest data ->> Trac Data
    TRAC CAP arrest -->> Trac Data
    TRAC arrests by arrest method ->> Trac Data
    Bob--x Alice: I am good thanks!
    Bob-x John: I am good thanks!
    Note right of John: Bob thinks a long<br/>long time, so long<br/>that the text does<br/>not fit on a row.

    Bob-->Alice: Checking with John...
    Alice->John: Yes... John, how are you?
```
