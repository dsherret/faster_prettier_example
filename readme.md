# Example - Faster Prettier with dprint-plugin-prettier

First CI run:

- prettier - ~40s
- dprint w/ dprint-plugin-prettier - ~34s

Second CI run using incremental cache:

- prettier - ~40s
- dprint w/ dprint-plugin-prettier - 0s
