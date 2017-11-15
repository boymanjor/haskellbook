module Learn where

-- Note: We reference y before it is declared.
-- The order of declarations not matter in source files.
-- The entire file is loaded at once.
x = 10 * 5 + y

myResult = x * 5

y = 10
