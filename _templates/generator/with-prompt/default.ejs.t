---
to: _templates/<%= name %>/<%= action || 'new' %>/default.ejs.t
---
---
inject: false
to: src/filename.ext
---
const hello = ```
Hello!
This is your first hygen template.

Learn what it can do here:

https://github.com/jondot/hygen
```

console.log(hello)


