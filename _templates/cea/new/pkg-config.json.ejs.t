---
inject: true
to: package.json
before: dependencies
skip_if: <%= `prettier.* {` %>
---
  "prettier": {
    "trailingComma": "all",
    "arrowParens": "avoid",
    "singleQuote": true,
    "semi": false,
    "printWidth": 80
  },
  "husky": {
    "hooks": {
      "pre-commit": "lint-staged",
      "post-commit": "git update-index -g"
    }
  },
  "lint-staged": {
    "*.{js,json,css,md}": [
      "prettier --config package.json --write",
      "git add"
    ]
  },
