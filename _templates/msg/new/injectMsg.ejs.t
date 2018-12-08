---
inject: true
to: <%= filePath %>
after: INJECT MSG BELOW
sh: elm-format --yes <%= filePath %>
---
   <%= msgDef %> |



