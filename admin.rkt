#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; the Acquire game administrator class: sign up players and manage their play 

(require "admin-intf.rkt") 

(admin& administrator% turn% DONE EXHAUSTED SCORE)
(provide Player% Turn% Administrator%)
;; ---------------------------------------------------------------------------------------------------

(require "typed-wrapper.rkt" "tree.rkt")
(require/typed/provide "player0-apply-wrapper.rkt"
                       [build-player0 (String (Listof Tile) (List Any (Instance Player%)) -> Player)])

#;(module+ test 
    (require rackunit (submod "state.rkt" sample-states) (submod "board.rkt" tiles+spots)))

(define-type Player%
  (Class 
   (init-field [name String]
               [choice Strategy])
   #;(field [*players (Listof Player)]
          [*bad (Listof Player)])
   [go ((Instance Administrator%) -> Any)]
   [setup (State -> Any)]
   [take-turn ((Instance Turn-Player%) -> (Values (Option Tile)
                                                  (Option Hotel)
                                                  Shares-Order))]
   [keep ((Listof Hotel) -> (Listof Boolean))]
   [receive-tile (Tile -> Any)]
   [inform [State -> Any]]
   [the-end (State Score -> Any)]))

(define-type Administrator%
  (Class
   (init-field [next-tile ((Listof Tile) -> Tile)])
   [sign-up (String (Instance Player%) -> String)]
   [show-players (-> (Listof String))]
   [run (Natural [#:show (Natural State -> Any)] -> (Values Symbol Any (Listof State)))]))
   
(: administrator% Administrator%)
(define administrator%
  (class object% 
    (init-field next-tile)
    (super-new)
    
    (: *count Integer)
    (define *count 10)
    (: *named-players (Listof (List String (Instance Player%))))
    (define *named-players '())
    ;; effect: keep track of players 
    (define/public (sign-up name player)
      (define pre (if (< (random 100) 50) "player" "spieler"))
      (set! *count (+ *count 1 (random 10)))
      (set! name (format "~a~a:~a" pre (number->string *count) name))
      (set! *named-players (cons (list name player) *named-players))
      name)
    
    (define/public (show-players)
      ((inst map String (List String (Instance Player%))) first *named-players))
    
    (define/public (run turns# #:show (show void))
      (define players (player->internals))
      (when (empty? players) (error 'run "players failed to sign up properly"))
      (: tree0 (Instance ATree%))
      (define tree0 (generate-tree (setup-all players (apply state0 players))))
      ;; generative recursion: n to 0, but terminate before if final state is reached
      ;; accumulator: states is reverese list of states encountered since tile distribution 
      (let: loop : (Values Symbol Any (Listof State)) ([tree tree0][n : Natural turns#][states (list (tree-state tree0))])
        (define state (tree-state tree))
        (cond
          [(= n 0)
           (values DONE (state-score state) (reverse states))]
          [(empty? (state-players state))
           (values EXHAUSTED (state-score state) (reverse states))]
          [(not (decision-tree? tree))
           (finish state)
           (values SCORE (state-score state) (reverse states))]
          [else 
           (: external (Instance Player%))
           (define external (cast (player-external (state-current-player state)) (Instance Player%)))
           (show n state)
           (define turn (new turn% [current-state state]))
           (in-sandbox-2
            #:time (* (+ (length (state-players state)) 1) 3)
            (lambda () (send external take-turn turn))
            ;; success
            (lambda: ([tile : (Option Tile)] [hotel-involved : (Option Hotel)] [buy-shares : Shares-Order])
              (cond
                [(boolean? tile) 
                 (finish state)
                 (values IMPOSSIBLE (state-score state) (reverse (cons state states)))]
                [else 
                 (define merger? (eq? (what-kind-of-spot (state-board state) tile) MERGING))
                 (cond 
                   ;; -------------------------------------------------------------------------------
                   ;; temporal contract: 
                   [(and merger? (not (send turn place-called)))
                    (loop (generate-tree (state-remove-current-player state)) (- n 1) states)]
                   ;; -------------------------------------------------------------------------------
                   [else  
                    (define-values (t1 h1 d*) (if merger? (send turn decisions) (values #f #f '())))
                    ;; assert: if merging?:
                    (and (equal? tile t1) (equal? hotel-involved h1))
                    (define eliminate (send turn eliminated))
                    (cond
                      [(member (state-current-player state) eliminate)
                       ((failure state states (lambda: ([s : (Instance ATree%)]) (loop s  (- n 1) states)))
                        `(S  "current player failed on keep"))]
                      [else 
                       (define tree/eliminate 
                         (if (empty? eliminate)
                             tree
                             (generate-tree (state-eliminate state eliminate))))
                       (exec external (cast tree/eliminate (Instance LPlaced%)) tile hotel-involved d* buy-shares
                             (lambda: ([next-tree : (Instance ATree%)] [state : State])
                               (inform-all 
                                next-tree state
                                (lambda: ([next-tree : (Instance ATree%)] [state : State])
                                  (cond
                                    [(empty? (state-tiles state))
                                     (finish state)
                                     (values EXHAUSTED (state-score state) (reverse states))]
                                    [else (loop next-tree (- n 1) (cons state states))]))))
                             ;; failure 
                             (failure state states (lambda (s) (loop s  (- n 1) states))))])])]))
            ;; failure: 
            (failure state states (lambda (s) (loop s  (- n 1) states))))])))
    
    (: failure (State (Listof State) ((Instance ATree%) -> (Values Symbol Any (Listof State))) -> ((List Any Any) -> (Values Symbol Any (Listof State)))))
    (define/private ((failure state states continue) status)
      ;; this should be a logging action
      (log status `(turn failure  ,(player-name (state-current-player state))))
      (define state/eliminate (state-remove-current-player state))
      (if (empty? (state-players state/eliminate))
          (values EXHAUSTED '(all players failed) (reverse states))
          (continue (generate-tree state/eliminate)))) 
    
    ;; [ (cons Tile [Listof Tile]) -> Tile ] -> Tree Tile [Maybe Hotel] Decisions [Listof Hotel]
    ;; (Any -> Any) -- success continuation 
    ;; (Any -> Any) -- failure continuation 
    ;; -> Tree
    (: exec : (Instance Player%) (Instance LPlaced%) Tile (Option Hotel) Decisions Shares-Order ((Instance ATree%) State -> (Values Any Any Any)) ((List Any Any) -> (Values Symbol Any (Listof State))) -> (Values Symbol Any (Listof State)))
    (define/private (exec external tree0 placement hotel decisions shares-to-buy succ fail)
      (define-values (tile tree) (tree-next tree0 placement hotel decisions shares-to-buy next-tile))
      (in-sandbox-3 (lambda () (send external receive-tile (assert tile)))
                  (lambda (_) (succ tree (tree-state tree)))
                  fail))
    
    ;; State (Tree State -> Any)  -> Any 
    (: inform-all ((Instance ATree%) State ((Instance ATree%) State -> (Values Any Any Any)) -> (Values Any Any Any)))
    (define/private (inform-all tree state k)
      (define eliminate
        (for/fold:  : (Listof Player) ((throw-out : (Listof Player) '())) ((p (state-players state)))
          (in-sandbox-1 (lambda () (send (cast (player-external p) (Instance Player%)) inform state))
                      (lambda (_) throw-out)
                      (lambda: ([status : (List Any Any)])
                        (log status `(inform ,(player-name p)))
                        (cons p throw-out)))))
      (cond
        [(empty? eliminate) (k tree state)]
        [else (define state/eliminate (state-eliminate state eliminate))
              (k (generate-tree state/eliminate) state/eliminate)]))
    
    ;; -> [Listof InternalPlayer]
    ;; create internal players for each external player
    (: player->internals (-> (Listof Player)))
    (define/private (player->internals)
      #;(define-values (internals _)
          (for/fold: : (Values Any Any) ((internals '()) (tile* ALL-TILES)) ((name+eplayer *named-players))
            (define name (first name+eplayer))
            (define tiles (take tile* STARTER-TILES#))
            (define player (apply player0 name (append tiles (list (second name+eplayer)))))
            (values (cons player internals) (drop tile* STARTER-TILES#))))
      (: internals (Listof Player))
      (define internals
        (car (for/fold: : (Pairof (Listof Player) (Listof Tile))
               ((internals+tile* : (Pairof (Listof Player) (Listof Tile)) (cons '() ALL-TILES)))
               ((name+eplayer : (List String (Instance Player%)) *named-players))
               (: name String)
               (define name (first name+eplayer))
               (define tiles (take (cdr internals+tile*) STARTER-TILES#))
               (define player (build-player0 name tiles name+eplayer) #;(apply player0 name (append tiles (list (second name+eplayer)))))
               (cons (cons player (car internals+tile*)) (drop (cdr internals+tile*) STARTER-TILES#)))))
      
      internals)
    
    ;; [Listof Player] State -> State
    (: setup-all ((Listof Player) State -> State))
    (define/private (setup-all players state)
      (define misbehaving
        (for/fold: : (Listof Player) ((misbehaving : (Listof Player) '())) ((p players))
          (in-sandbox-1 (lambda () (send (cast (player-external p) (Instance Player%)) setup state))
                        (lambda (_) misbehaving)
                        (lambda: ([status : (List Any Any)])
                          (log status `(setup ,(player-name p)))
                          (cons p misbehaving))) ))
      (if (empty? misbehaving) state (state-eliminate state misbehaving)))
    
    ;; State -> Void 
    ;; score the final state and send the final state and the score board to the players 
    (: finish (State -> Void))
    (define/private (finish state)
      (define score (state-score state))
      (for: ((e : Player (state-players state)))
        (in-sandbox-1 (lambda () (send (cast (player-external e) (Instance Player%)) the-end state score)) 
                    void
                    (lambda: ([status : (List Any Any)])
                      (log status `(end game ,(player-name e)))))))
    ))

(define DONE 'done)
(define EXHAUSTED 'exhausted)
(define SCORE 'score)

#;(define turn-administrator/c
  (class/c 
   (eliminated
    ;; the players that have been eliminated during callbacks
    (->m (listof player?)))
   
   ;; ------------------------------------------------------------------------------------------------
   ;; temporal contract
   (place-called
    ;; how often was the place method in this turn called
    (->m boolean?))
   
   ;; -------------------------------------------------------------------------------------------------
   (decisions
    ;; what are the decisions that the players made when place in this turn was called 
    ;; -----------------------------------------------------------------------------------------------
    ;; temporal contract: #:pre
    ;; -----------------------------------------------------------------------------------------------
    (->dm () #:pre (send this place-called)
          (values (tile tile?)
                  (hotel hotel?) 
                  (decisions (listof (list/c player? (listof (list/c hotel? boolean?))))))))))



(define-type Turn%
  (Class #:implements Turn-Administrator%
         (init-field [current-state State])
         (field [board Board]
                [current Player]
                [cash Cash]
                [tiles (Listof Tile)]
                [hotels (Listof Hotel)]
                [players (Listof Player)])))

                
;(define/contract turn%
;  turn-administrator/c
; might need more specific type
(: turn% Turn-Player%)
(define turn%
  (class object% 
    (init-field current-state)
    
    (field 
     [board   (state-board current-state)]
     [current (state-current-player current-state)]
     [cash    (player-money current)]
     [tiles   (player-tiles current)]
     [shares  (state-shares current-state)]
     [hotels  (state-hotels current-state)]
     [players (state-players current-state)])
    
    (super-new)
    
    (define/public (reconcile-shares t)
      ;; in principle: 
      ;; (error 'reconcile-shares "not possible for local game play")
      ;; but I need to accommodate game testing
      t)
    
    (define/public (eliminated)
      *eliminated)
    
    (define/public (decisions)
      (values (assert *tile) (assert *hotel) *decisions))
    
    (define/public (place-called)
      *called)
    (: *tile (Option Tile))
    (define *tile #f)
    (: *hotel (Option Hotel))
    (define *hotel #f)
    (: *decisions Decisions)
    (define *decisions '())
    (: *eliminated (Listof Player))
    (define *eliminated '())
    (: *called Boolean)
    (define *called #f)
    ;; effect: increments *called, sets *decisions to players' decisions
    (define/public (place tile hotel)
      ;; -------------------------
      ;; temporal contract
      (unless *called
        (set! *called #t)
        (set! *tile tile)
        (set! *hotel hotel)
        ;; -------------------------
        (define-values (acquirers acquired) (merging-which board tile))
        (define acquired-hotels (append (remove hotel acquirers) acquired))
        
        ;; determine decisions and update *decisions 
        (let: keep-to-all : Any ((players : (Listof Player) players))
          (unless (empty? players)
            (: p Player)
            (define p (first players))
            (in-sandbox-1 
             (lambda () 
               (define ex (player-external p))
               (if (boolean? ex)
                   ;; accommodate fake players, and say that they always keep all shares 
                   (map (lambda (_) #t) acquired-hotels)
                   (send (cast ex (Instance Player%)) keep acquired-hotels)))
             (lambda: ([p-s-decisions : (Listof Boolean)])
               (set! *decisions (cons (list p ((inst map (List Hotel Boolean) Hotel Boolean) (lambda: ([x : Hotel] [y : Boolean]) (list x y)) acquired-hotels p-s-decisions)) *decisions))
               (keep-to-all (rest players)))
             (lambda: ([status : (List Any Any)])
               (log status `(keep failure ,(player-name p)))
               (set! *eliminated (cons p *eliminated))
               (keep-to-all (rest players))))))
        
        ;; now change state and let the current player know 
        (define state/eliminated (state-eliminate current-state *eliminated))
        (define state/returns (state-return-shares state/eliminated *decisions))
        ;; interesting question: could a return blow up?
        (set! current-state state/returns)
        (state-players state/returns)))))

#;(module+ test 
    ;; --- the current player gets a bonus from the merger that enables it to buy shares 
    (define state 
      (s3-merge-player-gets-bonus-can-buy (shares++ (shares++ player-shares0 TOWER) TOWER)))
    (define turn (new turn% [current-state state]))
    (check-true ((good-shares turn C3 TOWER) (list TOWER TOWER)))
    
    ;; --- players return shares during the keep action that the current player can acquire
    (define turn-subtle
      (new (class object% 
             (field (current-state
                     (state-place-tile 
                      (s3-merge-player-gets-bonus-can-buy (shares++ player-shares0 CONTINENTAL))
                      C3 TOWER)))
             (super-new)
             (define/public (place-called) #t))))
    (check-true ((good-shares turn-subtle C3 TOWER) (list CONTINENTAL))))
