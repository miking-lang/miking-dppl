-- include "parser output"


-- tool output
lang  =  
 
-- AST of simple program containing only the flip statemen t
let ast = File {
  decl = [
    FunctionDecl {
      name = {
        v = nameNoSym "flip",
        i = NoInfo ()
      },
      info = NoInfo (),
      model = None (),
      args = [
        {
          ty = TypeConstructorType {
            name = {
              v = nameNoSym "Real",
              i = NoInfo ()
            },
            info = NoInfo ()
          },
          name = {
            v = nameNoSym "p",
            i = NoInfo ()
          }
        }
      ],
      returnTy = Some (TypeConstructorType {
            name = {
              v = nameNoSym "Bool",
              i = NoInfo ()
            },
            info = NoInfo ()
          }),
      body = [
        AssumeStmnt {
          info = NoInfo (),
          randomVar = {
              v = nameNoSym "e",
              i = NoInfo ()
            },
          distribution = BernoulliExpr {
            prob = {
              info = NoInfo (),
              ident = {
                v = nameNoSym "p",
                i = NoInfo ()
              }
            }
          }
        },
        --- return statement excercise
      ]
    } 
  ],
  info = NoInfo ()
}

