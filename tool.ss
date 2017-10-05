(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss"))
  (provide tool@)
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      (define phase1 void)
      (define phase2 void))))
