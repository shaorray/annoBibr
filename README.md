
`annoBibr` Bibliography annotation tool

---

#### Annotate R Markdown notes

In the following style, notes will be structured into R6 objects:
```
[title] 
An example article title ends with a newline (special punctuates are allowed, only except for the concatenated the dollar mark and "title")

[tag]
semi-colon separated; projects; lab or techniques

[group]
areas of study; domains of knowledge

[quote]
Sentences from the article.

[?] A question to the quote, available in "questions" list to choose from.
```

A reference card can be viewed by seletion in console, e.g. `ntoe_list[[2]]`

<img width="956" alt="Screenshot 2022-07-31 at 22 42 29" src="https://user-images.githubusercontent.com/8829224/182044589-96902912-3196-409f-94de-d856edbee259.png">


search() function will subset the note entries by keyword matches.
```
result_list <- search(note_list, "keyword")
```

Pubmed cross-reference will fetch and append `year`, `authors`, and `abstract` to a note object.
```
note_one$pubmed()
```
