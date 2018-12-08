---
inject: true
to: <%= filePath %>
after: INJECT UPDATE CASE BELOW
sh: elm-format --yes <%= filePath %>
---
        <%= msgDef %> ->
            identity



