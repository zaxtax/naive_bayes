lam $ \ topic_prior36 ->
lam $ \ word_prior37 ->
lam $ \ z38 ->
lam $ \ w39 ->
lam $ \ doc40 ->
lam $ \ docUpdate41 ->
let_ (size w39) $ \ x42 ->
let_ (size z38) $ \ x43 ->
let_ (summate (nat_ 0)
              x43
              (\ i1344 ->
               let_ (z38 ! i1344) $ \ x45 ->
               let_ (x45 < nat_ 0) $ \ x46 ->
               let_ (case_ x46
                           [branch ptrue (nat_ 0), branch pfalse (nat_ 1)]) $ \ x47 ->
               let_ (i1344 == docUpdate41) $ \ x48 ->
               case_ x48 [branch ptrue (nat_ 0), branch pfalse (x47)])) $ \ x49 ->
let_ (nat2prob x49) $ \ x50 ->
let_ (size word_prior37) $ \ x51 ->
let_ (summate (nat_ 0)
              x51
              (\ i1352 -> word_prior37 ! i1352)) $ \ x53 ->
let_ (size topic_prior36) $ \ x54 ->
let_ (summate (nat_ 0)
              x54
              (\ i1355 -> topic_prior36 ! i1355)) $ \ x56 ->
let_ (x50 + x56) $ \ x57 ->
let_ (recip x57) $ \ x58 ->
(array x54 $
       \ zNew859 ->
       let_ (product (nat_ 0)
                     x54
                     (\ i60 ->
                      let_ (summate (nat_ 0)
                                    x42
                                    (\ i1361 ->
                                     let_ (doc40 ! i1361) $ \ x62 ->
                                     let_ (z38 ! x62) $ \ x63 ->
                                     let_ (i60 == x63) $ \ x64 ->
                                     let_ (w39 ! i1361) $ \ x65 ->
                                     let_ (x65 < nat_ 0) $ \ x66 ->
                                     let_ (not x66) $ \ x67 ->
                                     let_ (x67 && x64) $ \ x68 ->
                                     let_ (case_ x68
                                                 [branch ptrue (nat_ 1),
                                                  branch pfalse (nat_ 0)]) $ \ x69 ->
                                     let_ (x62 == docUpdate41) $ \ x70 ->
                                     case_ x70
                                           [branch ptrue (nat_ 0), branch pfalse (x69)])) $ \ x71 ->
                      let_ (nat2prob x71) $ \ x72 ->
                      let_ (i60 == zNew859) $ \ x73 ->
                      let_ (summate (nat_ 0)
                                    x42
                                    (\ i1374 ->
                                     let_ (w39 ! i1374) $ \ x75 ->
                                     let_ (x75 < nat_ 0) $ \ x76 ->
                                     let_ (not x76) $ \ x77 ->
                                     let_ (x77 && x73) $ \ x78 ->
                                     let_ (case_ x78
                                                 [branch ptrue (nat_ 1),
                                                  branch pfalse (nat_ 0)]) $ \ x79 ->
                                     let_ (doc40 ! i1374) $ \ x80 ->
                                     let_ (docUpdate41 == x80) $ \ x81 ->
                                     case_ x81
                                           [branch ptrue (x79), branch pfalse (nat_ 0)])) $ \ x82 ->
                      product (nat_ 0)
                              x82
                              (\ i1883 ->
                               let_ (nat2prob i1883) $ \ x84 ->
                               x72 +
                               x84 +
                               x53))) $ \ x85 ->
       let_ (recip x85) $ \ x86 ->
       let_ (topic_prior36 ! zNew859) $ \ x87 ->
       let_ (summate (nat_ 0)
                     x43
                     (\ i1388 ->
                      let_ (z38 ! i1388) $ \ x89 ->
                      let_ (zNew859 == x89) $ \ x90 ->
                      let_ (case_ x90
                                  [branch ptrue (nat_ 1), branch pfalse (nat_ 0)]) $ \ x91 ->
                      let_ (i1388 == docUpdate41) $ \ x92 ->
                      case_ x92 [branch ptrue (nat_ 0), branch pfalse (x91)])) $ \ x93 ->
       let_ (nat2prob x93) $ \ x94 ->
       let_ (x94 + x87) $ \ x95 ->
       product (nat_ 0)
               x54
               (\ i96 ->
                let_ (i96 == zNew859) $ \ x97 ->
                product (nat_ 0)
                        x51
                        (\ i1898 ->
                         let_ (word_prior37 ! i1898) $ \ x99 ->
                         let_ (summate (nat_ 0)
                                       x42
                                       (\ i13100 ->
                                        let_ (w39 ! i13100) $ \ x101 ->
                                        let_ (i1898 == x101) $ \ x102 ->
                                        let_ (doc40 ! i13100) $ \ x103 ->
                                        let_ (z38 ! x103) $ \ x104 ->
                                        let_ (i96 == x104) $ \ x105 ->
                                        let_ (x105 && x102) $ \ x106 ->
                                        let_ (case_ x106
                                                    [branch ptrue (nat_ 1),
                                                     branch pfalse (nat_ 0)]) $ \ x107 ->
                                        let_ (x103 == docUpdate41) $ \ x108 ->
                                        case_ x108
                                              [branch ptrue (nat_ 0),
                                               branch pfalse (x107)])) $ \ x109 ->
                         let_ (nat2prob x109) $ \ x110 ->
                         let_ (summate (nat_ 0)
                                       x42
                                       (\ i13111 ->
                                        let_ (w39 ! i13111) $ \ x112 ->
                                        let_ (i1898 == x112) $ \ x113 ->
                                        let_ (x97 && x113) $ \ x114 ->
                                        let_ (case_ x114
                                                    [branch ptrue (nat_ 1),
                                                     branch pfalse (nat_ 0)]) $ \ x115 ->
                                        let_ (doc40 ! i13111) $ \ x116 ->
                                        let_ (docUpdate41 == x116) $ \ x117 ->
                                        case_ x117
                                              [branch ptrue (x115),
                                               branch pfalse (nat_ 0)])) $ \ x118 ->
                         product (nat_ 0)
                                 x118
                                 (\ j119 ->
                                  let_ (nat2prob j119) $ \ x120 ->
                                  let_ (x110 + x120 + x99) $ \ x121 ->
                                  x121 *
                                  x95 *
                                  x58 *
                                  x86))))
