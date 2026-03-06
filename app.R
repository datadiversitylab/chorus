library(shiny)
library(DT)

store <- new.env(parent = emptyenv())
store$conferences   <- new.env(parent = emptyenv())
store$tokens        <- new.env(parent = emptyenv())
store$admin_keys    <- new.env(parent = emptyenv())
store$user_counts   <- new.env(parent = emptyenv())
store$conf_triggers <- new.env(parent = emptyenv())
store$activity_log  <- new.env(parent = emptyenv())
store$guests        <- new.env(parent = emptyenv())
store$kick_triggers <- new.env(parent = emptyenv())
store$vote_ledger   <- new.env(parent = emptyenv())

`%||%` <- function(a, b) if (!is.null(a)) a else b

get_trigger <- function(cid) {
  if (!exists(cid, envir = store$conf_triggers, inherits = FALSE))
    assign(cid, reactiveVal(0L), envir = store$conf_triggers)
  get(cid, envir = store$conf_triggers, inherits = FALSE)
}
bump <- function(cid) { t <- get_trigger(cid); t(t() + 1L) }

get_kick_trig <- function(sid) {
  if (!exists(sid, envir = store$kick_triggers, inherits = FALSE))
    assign(sid, reactiveVal(FALSE), envir = store$kick_triggers)
  get(sid, envir = store$kick_triggers, inherits = FALSE)
}

get_conf <- function(cid) {
  if (!exists(cid, envir = store$conferences, inherits = FALSE)) return(NULL)
  get(cid, envir = store$conferences, inherits = FALSE)
}

init_conf <- function(cid, admin_key) {
  e <- new.env(parent = emptyenv())
  e$questions    <- new.env(parent = emptyenv())
  e$settings     <- list(
    max_votes_total   = Inf,
    max_votes_per_q   = 1L,
    max_answers_per_q = 1L,
    max_answers_total = Inf,
    anonymous         = FALSE,
    show_results      = TRUE
  )
  e$admin_key    <- admin_key
  e$forced_qid   <- NULL
  assign(cid,       e,         envir = store$conferences)
  assign(cid,       0L,        envir = store$user_counts)
  assign(cid,       list(),    envir = store$activity_log)
  assign(cid,       list(),    envir = store$guests)
  assign(admin_key, cid,       envir = store$admin_keys)
  invisible(e)
}

add_activity <- function(cid, msg, msg_anon = NULL, admin_only = FALSE) {
  if (!exists(cid, envir = store$activity_log, inherits = FALSE)) return()
  log <- get(cid, envir = store$activity_log, inherits = FALSE)
  log <- c(list(list(msg = msg, msg_anon = msg_anon, admin_only = admin_only, ts = format(Sys.time(), "%H:%M"))), log)
  if (length(log) > 60) log <- log[seq_len(60)]
  assign(cid, log, envir = store$activity_log)
}

register_guest <- function(cid, sid, name) {
  gl <- get(cid, envir = store$guests, inherits = FALSE)
  gl[[sid]] <- list(name = name, kicked = FALSE)
  assign(cid, gl, envir = store$guests)
}
update_guest_name_global <- function(cid, sid, new_name) {
  gl <- get(cid, envir = store$guests, inherits = FALSE)
  if (!is.null(gl[[sid]])) gl[[sid]]$name <- new_name
  assign(cid, gl, envir = store$guests)
}
remove_guest <- function(cid, sid) {
  gl <- get(cid, envir = store$guests, inherits = FALSE)
  gl[[sid]] <- NULL
  assign(cid, gl, envir = store$guests)
  cnt <- max(0L, get(cid, envir = store$user_counts, inherits = FALSE) - 1L)
  assign(cid, cnt, envir = store$user_counts)
}
list_guests <- function(cid) {
  if (!exists(cid, envir = store$guests, inherits = FALSE)) return(list())
  get(cid, envir = store$guests, inherits = FALSE)
}

ledger_key  <- function(cid, sid, qid) paste(cid, sid, qid, sep = "|")

get_votes_for <- function(cid, sid, qid) {
  k <- ledger_key(cid, sid, qid)
  if (!exists(k, envir = store$vote_ledger, inherits = FALSE))
    return(setNames(integer(), character()))
  get(k, envir = store$vote_ledger, inherits = FALSE)
}
set_votes_for <- function(cid, sid, qid, votes) {
  assign(ledger_key(cid, sid, qid), votes, envir = store$vote_ledger)
}

total_votes_cast <- function(cid, sid) {
  all_keys <- ls(store$vote_ledger)
  prefix   <- paste0(cid, "|", sid, "|")
  relevant <- all_keys[startsWith(all_keys, prefix)]
  if (length(relevant) == 0) return(0L)
  sum(unlist(lapply(relevant, function(k)
    sum(get(k, envir = store$vote_ledger, inherits = FALSE), na.rm = TRUE)
  )), na.rm = TRUE)
}

refund_answer_votes <- function(cid, qid, aid_char) {
  all_keys <- ls(store$vote_ledger)
  prefix   <- paste0(cid, "|")
  q_suffix <- paste0("|", qid)
  relevant <- all_keys[startsWith(all_keys, prefix) & endsWith(all_keys, q_suffix)]
  refunds  <- list()
  for (k in relevant) {
    votes <- get(k, envir = store$vote_ledger, inherits = FALSE)
    if (aid_char %in% names(votes) && !is.na(votes[aid_char]) && votes[aid_char] > 0) {
      refund_n        <- votes[aid_char]
      votes[aid_char] <- 0L
      assign(k, votes, envir = store$vote_ledger)
      parts  <- strsplit(k, "\\|")[[1]]
      sid_ex <- paste(parts[2:(length(parts)-1)], collapse = "|")
      refunds[[sid_ex]] <- (refunds[[sid_ex]] %||% 0L) + refund_n
    }
  }
  refunds
}

within_total_limit <- function(total_cast, max_total) is.infinite(max_total) || total_cast < max_total
within_q_limit     <- function(q_cast,     max_per_q) is.infinite(max_per_q)  || q_cast  < max_per_q

make_timer_ui <- function(q, admin = FALSE) {
  active  <- isTRUE(q$timer_active)
  has_end <- !is.null(q$timer_end)
  dur     <- q$timer_secs %||% 0L
  
  if (!active && is.null(q$timer_end) && dur == 0L) return(NULL)
  
  if (active && has_end) {
    remaining <- max(0, as.numeric(difftime(q$timer_end, Sys.time(), units = "secs")))
    pct       <- if (dur > 0) round(remaining / dur * 100) else 0
    bar_cls   <- if (pct > 40) "timer-bar ok" else if (pct > 15) "timer-bar warn" else "timer-bar urgent"
    mins      <- floor(remaining / 60)
    secs      <- round(remaining %% 60)
    lbl       <- if (mins > 0) sprintf("%d:%02d", mins, secs) else sprintf("%ds", secs)
    tags$div(class = "timer-wrap",
             tags$div(class = "timer-lbl",
                      tags$span(class = "t-badge", lbl),
                      tags$span(class = "t-state", "Running")
             ),
             tags$div(class = "timer-bar-bg",
                      tags$div(class = bar_cls, style = paste0("width:", pct, "%;"))
             )
    )
  } else if (!active && dur > 0) {
    tags$div(class = "timer-expired", "Time is up. Voting has closed for this question.")
  } else NULL
}


chorus_css <- '
@import url("https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght@0,9..144,300;0,9..144,600;0,9..144,800;1,9..144,300;1,9..144,600&family=Figtree:wght@300;400;500;600;700&display=swap");

:root {
  --cream:#f8f5ef; --cream-dark:#ede8de;
  --ink:#1c2b22; --ink-mid:#3d5044; --ink-light:#6b836f;
  --sage:#7aab85; --sage-d:#5a8f6a; --sage-light:#a8c9b0;
  --sage-pale:#dff0e4; --sage-mist:#eef7f0;
  --gold:#c8a84b; --red-soft:#c0544a; --red-mist:#fdf0ee;
  --white:#ffffff; --border:#dce8df;
  --r-lg:18px; --r-md:12px; --r-sm:8px; --r-pill:999px;
  --sh-sm:0 2px 10px rgba(28,43,34,.07); --sh-md:0 6px 24px rgba(28,43,34,.11);
  --ease:0.2s cubic-bezier(.4,0,.2,1);
}
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0;}
html{scroll-behavior:smooth;font-size:17.5px;}
body{font-family:"Figtree",sans-serif!important;background:var(--cream)!important;
  color:var(--ink)!important;min-height:100vh;line-height:1.6;}
body::before{content:"";position:fixed;inset:0;z-index:0;pointer-events:none;
  background-image:
    radial-gradient(ellipse 80% 60% at 10% 5%,rgba(122,171,133,.09) 0%,transparent 60%),
    radial-gradient(ellipse 60% 50% at 90% 90%,rgba(200,168,75,.06) 0%,transparent 60%);}
.container-fluid{padding:0!important;position:relative;z-index:1;}

/* TOPBAR */
.topbar{position:sticky;top:0;z-index:200;display:flex;align-items:center;
  justify-content:space-between;padding:0 2rem;height:58px;
  background:rgba(248,245,239,.94);backdrop-filter:blur(20px) saturate(1.4);
  border-bottom:1px solid var(--border);}
.brand{display:flex;align-items:baseline;gap:.45rem;}
.brand-name{font-family:"Fraunces",serif;font-size:1.35rem;font-weight:800;
  letter-spacing:-.03em;color:var(--ink);}
.brand-tag{font-size:.68rem;font-weight:600;letter-spacing:.09em;
  text-transform:uppercase;color:var(--ink-light);}
.topbar-right{display:flex;align-items:center;gap:.75rem;}
.role-lbl{font-size:.72rem;font-weight:600;color:var(--ink-light);
  text-transform:uppercase;letter-spacing:.09em;}

/* WRAPPER */
.chorus-wrap{max-width:900px;margin:0 auto;padding:1.6rem 1.5rem 5rem;}

/* COMPACT HERO */
.hero{display:flex;align-items:center;justify-content:space-between;
  padding:.9rem 0 1.2rem;border-bottom:1px solid var(--border);margin-bottom:1.5rem;}
.hero-left{}
.eyebrow{font-size:.64rem;font-weight:700;letter-spacing:.18em;
  text-transform:uppercase;color:var(--sage);margin-bottom:.15rem;}
.hero-title{font-family:"Fraunces",serif;font-size:1.55rem;
  font-weight:800;line-height:1.1;color:var(--ink);letter-spacing:-.03em;}
.hero-title em{font-style:italic;color:var(--sage);}

/* CARD */
.pcard{background:var(--white);border:1px solid var(--border);border-radius:var(--r-lg);
  padding:1.5rem 1.7rem;box-shadow:var(--sh-sm);margin-bottom:1.1rem;
  animation:fadeUp .4s ease both;transition:box-shadow var(--ease);}
.pcard:hover{box-shadow:var(--sh-md);}
.pcard-title{font-family:"Fraunces",serif;font-size:1.1rem;font-weight:700;
  color:var(--ink);letter-spacing:-.02em;margin-bottom:.2rem;}
.pcard-sub{font-size:.88rem;color:var(--ink-light);font-weight:300;
  line-height:1.6;margin-bottom:1.2rem;}

/* STEPS */
.steps{list-style:none;display:flex;flex-direction:column;gap:.5rem;margin-bottom:1.5rem;}
.steps li{display:flex;align-items:flex-start;gap:.7rem;
  font-size:.9rem;color:var(--ink-mid);line-height:1.5;}
.step-n{width:22px;height:22px;border-radius:50%;flex-shrink:0;
  background:var(--sage);color:white;font-size:.65rem;font-weight:700;
  display:flex;align-items:center;justify-content:center;margin-top:2px;}

/* INPUTS */
label{font-size:.7rem!important;font-weight:700!important;text-transform:uppercase!important;
  letter-spacing:.1em!important;color:var(--ink-light)!important;
  margin-bottom:.35rem!important;display:block!important;}
.form-control{border:1.5px solid var(--border)!important;border-radius:var(--r-sm)!important;
  background:var(--white)!important;font-family:"Figtree",sans-serif!important;
  font-size:1rem!important;color:var(--ink)!important;padding:.6rem .9rem!important;
  transition:border-color var(--ease),box-shadow var(--ease)!important;
  box-shadow:none!important;width:100%!important;}
.form-control:focus{border-color:var(--sage)!important;
  box-shadow:0 0 0 3px rgba(122,171,133,.18)!important;outline:none!important;}
.form-group{margin-bottom:1rem!important;}
.token-input .form-control{font-family:"Fraunces",serif!important;font-size:1.4rem!important;
  font-weight:800!important;letter-spacing:.22em!important;text-align:center!important;
  padding:.8rem 1rem!important;border-radius:var(--r-md)!important;}

/* LIMIT INPUTS */
.limit-row{display:flex;align-items:center;gap:.6rem;flex-wrap:wrap;margin-bottom:.75rem;}
.limit-row label{margin-bottom:0!important;white-space:nowrap;}
.limit-row .form-control{width:80px!important;}
.limit-badge{display:inline-flex;align-items:center;gap:.35rem;
  background:var(--sage-mist);border:1px solid var(--sage-pale);
  border-radius:var(--r-pill);padding:.25rem .75rem;
  font-size:.78rem;font-weight:600;color:var(--ink-mid);}
.limit-badge.unlimited{color:var(--sage-d);}

/* BUTTONS */
.btn{font-family:"Figtree",sans-serif!important;font-size:.85rem!important;
  font-weight:700!important;letter-spacing:.07em!important;text-transform:uppercase!important;
  border-radius:var(--r-pill)!important;padding:.58rem 1.5rem!important;
  border:none!important;cursor:pointer!important;transition:all var(--ease)!important;
  display:inline-flex!important;align-items:center!important;gap:.35rem!important;}
.btn-sage{background:var(--sage)!important;color:white!important;
  box-shadow:0 2px 8px rgba(122,171,133,.32)!important;}
.btn-sage:hover{background:var(--sage-d)!important;transform:translateY(-1px)!important;}
.btn-ink{background:var(--ink)!important;color:var(--cream)!important;
  box-shadow:0 2px 8px rgba(28,43,34,.2)!important;}
.btn-ink:hover{background:var(--ink-mid)!important;transform:translateY(-1px)!important;}
.btn-ghost{background:transparent!important;color:var(--ink-light)!important;
  border:1.5px solid var(--border)!important;}
.btn-ghost:hover{border-color:var(--sage)!important;color:var(--ink)!important;
  background:var(--sage-mist)!important;}
.btn-danger{background:var(--red-mist)!important;color:var(--red-soft)!important;
  border:1.5px solid rgba(192,84,74,.2)!important;}
.btn-danger:hover{background:rgba(192,84,74,.16)!important;}
.btn-sm{padding:.32rem .75rem!important;font-size:.72rem!important;}

/* LOCK BANNER */
.lock-banner{display:flex;align-items:center;gap:.5rem;
  background:#fff8e6;border:1px solid #f0d070;border-radius:var(--r-sm);
  padding:.55rem .9rem;font-size:.86rem;color:#7a5c00;margin-bottom:.9rem;}
.lock-icon{font-size:.95rem;}


.token-strip{background:var(--ink);border-radius:var(--r-md);
  padding:1.1rem 1.5rem;margin-bottom:1rem;
  display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:1rem;
  position:relative;overflow:hidden;animation:fadeUp .4s ease both;}
.token-strip::before{content:"";position:absolute;top:-30px;right:-30px;
  width:130px;height:130px;border-radius:50%;z-index:0;
  background:radial-gradient(circle,rgba(122,171,133,.2) 0%,transparent 70%);pointer-events:none;}
.ts-left{display:flex;align-items:center;gap:1.2rem;position:relative;z-index:1;}
.ts-token-block{}
.ts-room-lbl{font-size:.6rem;font-weight:700;text-transform:uppercase;
  letter-spacing:.14em;color:var(--sage-light);margin-bottom:.15rem;}
.ts-token-val{font-family:"Fraunces",serif;font-size:2rem;font-weight:800;
  letter-spacing:.22em;color:white;line-height:1;}
.ts-hint{font-size:.72rem;color:rgba(255,255,255,.38);margin-top:.2rem;font-weight:300;}
.ts-key-block{border-left:1px solid rgba(255,255,255,.12);padding-left:1.2rem;}
.ts-key-lbl{font-size:.6rem;font-weight:700;text-transform:uppercase;
  letter-spacing:.12em;color:rgba(255,255,255,.35);margin-bottom:.15rem;}
.ts-key-val{font-family:"Fraunces",serif;font-size:.88rem;font-weight:600;
  letter-spacing:.1em;color:rgba(255,255,255,.6);}
.ts-key-hint{font-size:.66rem;color:rgba(255,255,255,.25);margin-top:.1rem;}
.ts-right{display:flex;align-items:center;gap:.6rem;flex-shrink:0;position:relative;z-index:2;}

/* STATUS BAR */
.status-bar{display:inline-flex;align-items:center;gap:.5rem;
  padding:.45rem .95rem;background:var(--sage-mist);
  border-radius:var(--r-pill);border:1px solid var(--sage-pale);
  font-size:.83rem;font-weight:500;color:var(--ink-mid);margin-bottom:1.1rem;}
.status-dot{width:8px;height:8px;border-radius:50%;background:var(--sage);
  animation:pulse 2s ease-in-out infinite;}
@keyframes pulse{0%,100%{opacity:1;transform:scale(1)}50%{opacity:.4;transform:scale(.8)}}

/* ADMIN TABS */
.admin-tabs{display:flex;gap:.25rem;margin-bottom:1.3rem;
  background:var(--white);border:1px solid var(--border);
  border-radius:var(--r-pill);padding:.3rem;width:fit-content;}
.atab{font-family:"Figtree",sans-serif;font-size:.75rem;font-weight:700;
  letter-spacing:.07em;text-transform:uppercase;cursor:pointer;
  padding:.4rem 1.1rem;border-radius:var(--r-pill);border:none;
  background:transparent;color:var(--ink-light);
  transition:all var(--ease);}
.atab.active{background:var(--sage);color:white;
  box-shadow:0 2px 8px rgba(122,171,133,.35);}
.atab:hover:not(.active){background:var(--sage-mist);color:var(--ink);}
.tab-panel{display:none;}
.tab-panel.active{display:block;}

/* TWO-COL GRID */
.admin-grid{display:grid;grid-template-columns:1fr 1fr;gap:1.1rem;margin-bottom:1.1rem;}
@media(max-width:680px){.admin-grid{grid-template-columns:1fr;}}

/* SECTION LABELS */
.div{height:1px;background:var(--border);margin:1.1rem 0;}
.sec-eye{font-size:.62rem;font-weight:700;text-transform:uppercase;
  letter-spacing:.16em;color:var(--sage);margin-bottom:.35rem;}

/* SELF-RENAME */
.self-rename-row{display:flex;gap:.55rem;align-items:flex-end;margin-bottom:.9rem;}
.self-rename-row .form-control{font-size:.88rem!important;}

/* ANSWER CARDS */
.answer-list{display:flex;flex-direction:column;gap:.6rem;margin-top:.4rem;}
.answer-card{background:var(--white);border:1.5px solid var(--border);
  border-radius:var(--r-md);padding:.85rem 1rem;display:flex;align-items:center;gap:.9rem;
  transition:border-color var(--ease),box-shadow var(--ease),transform var(--ease);
  position:relative;overflow:hidden;}
.answer-card:hover{border-color:var(--sage-light);box-shadow:var(--sh-sm);transform:translateY(-1px);}
.answer-card.voted{border-color:var(--sage);background:var(--sage-mist);}
.answer-card.admin-card{border-color:var(--gold);background:#fffdf5;}
.answer-bar-bg{position:absolute;left:0;top:0;bottom:0;border-radius:var(--r-md);
  background:linear-gradient(90deg,rgba(122,171,133,.12),rgba(122,171,133,.03));
  transition:width .55s cubic-bezier(.4,0,.2,1);z-index:0;}
.answer-content{flex:1;position:relative;z-index:1;min-width:0;}
.answer-text{font-size:1rem;font-weight:500;color:var(--ink);line-height:1.4;}
.answer-meta{font-size:.78rem;color:var(--ink-light);margin-top:.1rem;}
.admin-tag{display:inline-block;font-size:.58rem;font-weight:700;
  text-transform:uppercase;letter-spacing:.08em;
  background:#fdf3d0;color:#b8860b;border-radius:99px;
  padding:.1rem .45rem;margin-left:.35rem;vertical-align:middle;}
.answer-vote-controls{display:flex;align-items:center;gap:.4rem;
  position:relative;z-index:1;flex-shrink:0;}
.vote-count{font-family:"Fraunces",serif;font-size:1.2rem;font-weight:700;
  color:var(--ink);min-width:2rem;text-align:center;line-height:1;}
.vbtn{width:34px;height:34px;border-radius:50%;border:none!important;
  cursor:pointer!important;font-size:1.1rem;font-weight:700;line-height:1;
  display:flex;align-items:center;justify-content:center;
  transition:all var(--ease)!important;padding:0!important;}
.vbtn-plus{background:var(--sage)!important;color:white!important;
  box-shadow:0 2px 6px rgba(122,171,133,.32)!important;}
.vbtn-plus:hover{background:var(--sage-d)!important;transform:scale(1.1)!important;}
.vbtn-minus{background:var(--cream-dark)!important;color:var(--red-soft)!important;}
.vbtn-minus:hover{background:#f5ddd9!important;transform:scale(1.1)!important;}
.vbtn-del{background:var(--red-mist)!important;color:var(--red-soft)!important;
  font-size:.8rem!important;width:28px!important;height:28px!important;}
.vbtn-del:hover{background:rgba(192,84,74,.18)!important;}

/* VOTE MSG / BUDGET */
.vote-msg{font-size:.84rem;font-weight:500;color:var(--red-soft);
  background:var(--red-mist);border-radius:var(--r-sm);padding:.5rem .9rem;
  margin-top:.7rem;border:1px solid rgba(192,84,74,.16);display:inline-block;}
.vote-budget{display:flex;gap:.6rem;flex-wrap:wrap;margin-bottom:.9rem;}
.budget-pill{display:inline-flex;align-items:center;gap:.3rem;padding:.3rem .8rem;
  border-radius:var(--r-pill);font-size:.78rem;font-weight:600;
  background:var(--sage-mist);border:1px solid var(--sage-pale);color:var(--ink-mid);}
.budget-pill.warn{background:#fff8e6;border-color:#f0d070;color:#7a5c00;}
.budget-dot{width:6px;height:6px;border-radius:50%;background:var(--sage);}
.budget-dot.warn{background:var(--gold);}

/* KICK / CLOSED SCREENS */
.kicked-screen,.closed-screen{text-align:center;padding:3rem 2rem;animation:fadeUp .4s ease both;}
.kicked-icon,.closed-icon{font-size:2.6rem;margin-bottom:.8rem;}
.kicked-title,.closed-title{font-family:"Fraunces",serif;font-size:1.6rem;font-weight:700;
  color:var(--ink);margin-bottom:.4rem;}
.kicked-sub,.closed-sub{font-size:.95rem;color:var(--ink-light);font-weight:300;}

/* NAME BADGE */
.name-badge{display:inline-flex;align-items:center;gap:.45rem;
  background:var(--sage-mist);border:1px solid var(--sage-pale);
  border-radius:var(--r-pill);padding:.35rem .9rem;
  font-size:.84rem;font-weight:500;color:var(--ink-mid);margin-bottom:.9rem;}
.name-badge-dot{font-size:.65rem;color:var(--sage);}

/* ACTIVITY FEED */
.activity-feed{background:var(--white);border:1px solid var(--border);
  border-radius:var(--r-md);padding:.9rem 1.1rem;max-height:220px;overflow-y:auto;
  display:flex;flex-direction:column;gap:.4rem;}
.activity-feed::-webkit-scrollbar{width:4px;}
.activity-feed::-webkit-scrollbar-thumb{background:var(--border);border-radius:4px;}
.activity-item{display:flex;gap:.55rem;align-items:flex-start;font-size:.82rem;
  line-height:1.4;padding-bottom:.4rem;border-bottom:1px solid var(--border);}
.activity-item:last-child{border-bottom:none;padding-bottom:0;}
.activity-ts{font-size:.68rem;color:var(--ink-light);flex-shrink:0;margin-top:1px;}
.activity-msg{color:var(--ink-mid);}
.activity-msg strong{color:var(--ink);font-weight:600;}
.activity-empty{font-size:.84rem;color:var(--ink-light);font-style:italic;padding:.25rem 0;}

/* GUEST TABLE */
.guest-table{width:100%;border-collapse:collapse;}
.guest-table th{font-size:.68rem;font-weight:700;text-transform:uppercase;
  letter-spacing:.1em;color:var(--ink-light);background:var(--sage-mist);
  border-bottom:1px solid var(--border);padding:.55rem .9rem;text-align:left;}
.guest-table td{font-size:.88rem;padding:.55rem .9rem;
  border-bottom:1px solid var(--border);vertical-align:middle;}
.guest-table tr:last-child td{border-bottom:none;}

/* TOP ANSWERS TABLE */
.topans-table{width:100%;border-collapse:collapse;}
.topans-table thead th{font-size:.68rem;font-weight:700;text-transform:uppercase;
  letter-spacing:.1em;color:var(--ink-light);background:var(--sage-mist);
  border-bottom:1px solid var(--border);padding:.65rem .9rem;text-align:left;}
.topans-table tbody tr:hover{background:var(--sage-mist);}
.topans-table tbody td{font-size:.88rem;color:var(--ink);
  padding:.65rem .9rem;border-bottom:1px solid var(--border);}
.topans-table tbody tr:last-child td{border-bottom:none;}
.rank-badge{display:inline-flex;align-items:center;justify-content:center;
  width:24px;height:24px;border-radius:50%;
  font-family:"Fraunces",serif;font-size:.8rem;font-weight:700;}
.rank-1{background:#fdf3d0;color:#b8860b;}
.rank-2{background:#f0f0f0;color:#666;}
.rank-3{background:#fde8d8;color:#c06020;}
.rank-n{background:var(--sage-mist);color:var(--ink-light);font-size:.72rem;}
.mini-bar-wrap{width:80px;height:7px;background:var(--cream-dark);border-radius:4px;overflow:hidden;}
.mini-bar{height:100%;background:linear-gradient(90deg,var(--sage),var(--sage-light));
  border-radius:4px;transition:width .6s ease;}

/* DT overrides */
table.dataTable{font-family:"Figtree",sans-serif!important;border-collapse:collapse!important;}
table.dataTable thead th{font-size:.72rem!important;font-weight:700!important;
  text-transform:uppercase!important;letter-spacing:.1em!important;
  color:var(--ink-light)!important;background:var(--sage-mist)!important;
  border-bottom:1px solid var(--border)!important;padding:.65rem .9rem!important;}
table.dataTable tbody td{font-size:.9rem!important;padding:.65rem .9rem!important;
  border-bottom:1px solid var(--border)!important;}
table.dataTable tbody tr:hover{background:var(--sage-mist)!important;}
.dataTables_wrapper{padding:0!important;}
.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_paginate{
  font-size:.78rem!important;font-family:"Figtree",sans-serif!important;
  color:var(--ink-light)!important;padding:.65rem 0!important;}

/* SELECTIZE */
.selectize-input{border:1.5px solid var(--border)!important;border-radius:var(--r-sm)!important;
  font-family:"Figtree",sans-serif!important;font-size:.95rem!important;
  color:var(--ink)!important;padding:.55rem .85rem!important;
  box-shadow:none!important;background:var(--white)!important;}
.selectize-input:focus-within{border-color:var(--sage)!important;
  box-shadow:0 0 0 3px rgba(122,171,133,.18)!important;}
.selectize-dropdown{font-family:"Figtree",sans-serif!important;font-size:.9rem!important;
  border-radius:var(--r-sm)!important;border:1.5px solid var(--border)!important;
  box-shadow:var(--sh-md)!important;}
.selectize-dropdown .option:hover,.selectize-dropdown .option.active{
  background:var(--sage-mist)!important;color:var(--ink)!important;}
.topbar-sel .selectize-input{border-radius:999px!important;
  padding:.32rem 1rem!important;font-size:.84rem!important;font-weight:600!important;}

/* MISC */
.well{background:transparent!important;border:none!important;
  box-shadow:none!important;padding:0!important;}
.checkbox label{font-size:.9rem!important;font-weight:400!important;
  text-transform:none!important;letter-spacing:0!important;color:var(--ink-mid)!important;}
input[type="checkbox"]{accent-color:var(--sage);transform:scale(1.15);}
.shiny-notification{font-family:"Figtree",sans-serif!important;font-size:.88rem!important;
  border-radius:var(--r-md)!important;border-left:4px solid var(--sage)!important;}

/* FOOTER */
.chorus-foot{text-align:center;padding:2rem;border-top:1px solid var(--border);
  margin-top:2rem;font-size:.8rem;color:var(--ink-light);font-weight:300;}
.chorus-foot a{color:var(--sage);text-decoration:none;font-weight:600;}
.chorus-foot a:hover{text-decoration:underline;}
.foot-note{margin-top:.5rem;font-size:.7rem;background:var(--sage-mist);
  border-radius:99px;padding:.32rem 1rem;display:inline-block;}

@keyframes fadeUp{from{opacity:0;transform:translateY(14px)}to{opacity:1;transform:translateY(0)}}

/* TIMER */
.timer-wrap{margin-bottom:1rem;}
.timer-bar-bg{height:7px;background:var(--cream-dark);border-radius:4px;overflow:hidden;margin-bottom:.4rem;}
.timer-bar{height:100%;border-radius:4px;transition:width 1s linear,background .5s ease;}
.timer-bar.ok{background:linear-gradient(90deg,var(--sage),var(--sage-light));}
.timer-bar.warn{background:linear-gradient(90deg,#e8a020,#f0c050);}
.timer-bar.urgent{background:linear-gradient(90deg,var(--red-soft),#e07060);}
.timer-lbl{font-size:.76rem;font-weight:700;color:var(--ink-mid);display:flex;justify-content:space-between;align-items:center;}
.timer-lbl .t-badge{font-family:"Fraunces",serif;font-size:.95rem;letter-spacing:.04em;}
.timer-lbl .t-state{font-size:.66rem;text-transform:uppercase;letter-spacing:.1em;color:var(--ink-light);}
.timer-expired{font-size:.84rem;font-weight:600;color:var(--red-soft);
  background:var(--red-mist);border:1px solid rgba(192,84,74,.2);
  border-radius:var(--r-sm);padding:.45rem .9rem;margin-bottom:.9rem;}

/* MODE BADGES */
.mode-badge{display:inline-flex;align-items:center;gap:.3rem;
  padding:.24rem .68rem;border-radius:var(--r-pill);
  font-size:.66rem;font-weight:700;letter-spacing:.08em;text-transform:uppercase;}
.mode-badge.anon{background:#ede8f8;color:#6040a0;border:1px solid #c8b8e8;}
.mode-badge.blind{background:#fdf3d0;color:#9a6800;border:1px solid #e8d070;}

/* BLIND MODE */
.answer-card.blind-mode .vote-count{display:none;}
.answer-card.blind-mode .answer-bar-bg{display:none!important;}
.answer-card.blind-mode .answer-meta .vote-pct{display:none;}

/* MY VOTE COUNT */
.my-vote-count{font-family:"Fraunces",serif;font-size:.78rem;font-weight:700;
  color:var(--sage-d);background:var(--sage-mist);border:1px solid var(--sage-pale);
  border-radius:var(--r-pill);padding:.12rem .5rem;min-width:1.5rem;text-align:center;}

/* CLOSE ROOM */
.btn-close-room{background:var(--red-mist)!important;color:var(--red-soft)!important;
  border:1.5px solid rgba(192,84,74,.25)!important;position:relative!important;z-index:2!important;}
.btn-close-room:hover{background:rgba(192,84,74,.16)!important;}

/* INLINE SECTION HEADER */
.inline-section{display:flex;align-items:center;justify-content:space-between;
  margin-bottom:.75rem;}


.q-header{margin-bottom:.75rem;}
.q-header .sec-eye{margin-bottom:.2rem;}
'

ui <- fluidPage(
  tags$head(
    tags$style(HTML(chorus_css)),
    tags$script(HTML("
$(document).on('shiny:value', function(e) {
  if (e.name === 'guest_answer_section' || e.name === 'guest_suggest_lock_ui' ||
      e.name === 'question_ui_guest' || e.name === 'guest_vote_budget_ui' ||
      e.name === 'guest_timer_ui' || e.name === 'guest_mode_badges_ui') {
    var el = document.getElementById('answer_text');
    if (el && !el._justSubmitted) { el._savedVal = el.value; }
  }
  if (e.name === 'question_ui_admin' || e.name === 'lock_toggle_ui') {
    var el2 = document.getElementById('admin_answer_text');
    if (el2 && !el2._justSubmitted) { el2._savedVal = el2.value; }
  }
});
$(document).on('shiny:visualchange', function(e) {
  var el = document.getElementById('answer_text');
  if (el && el._savedVal !== undefined && el.value !== el._savedVal) {
    el.value = el._savedVal; el._savedVal = undefined;
  }
  var el2 = document.getElementById('admin_answer_text');
  if (el2 && el2._savedVal !== undefined && el2.value !== el2._savedVal) {
    el2.value = el2._savedVal; el2._savedVal = undefined;
  }
});
Shiny.addCustomMessageHandler('clearAnswerInput', function(msg) {
  var el = document.getElementById('answer_text');
  if (el) {
    el._justSubmitted = true; el._savedVal = undefined; el.value = '';
    setTimeout(function() { el._justSubmitted = false; }, 2000);
  }
});
function showTab(id) {
  document.querySelectorAll('.tab-panel').forEach(function(p){ p.classList.remove('active'); });
  document.querySelectorAll('.atab').forEach(function(t){ t.classList.remove('active'); });
  var panel = document.getElementById('tab-' + id);
  if (panel) panel.classList.add('active');
  var btn = document.querySelector('[data-tab=\"' + id + '\"]');
  if (btn) btn.classList.add('active');
}
$(document).ready(function(){ showTab('settings'); });
    "))
  ),
  
  tags$div(class = "topbar",
           tags$div(class = "brand",
                    tags$span(class = "brand-name", "Chorus"),
                    tags$span(class = "brand-tag", "Live Polling")
           ),
           tags$div(class = "topbar-right",
                    tags$span(class = "role-lbl", "I am a"),
                    tags$div(class = "topbar-sel",
                             selectInput("user_role", label = NULL,
                                         choices = c("Guest", "Admin"), selected = "Guest", width = "140px")
                    )
           )
  ),
  
  tags$div(class = "chorus-wrap",
           
           tags$div(class = "hero",
                    tags$div(class = "hero-left",
                             tags$div(class = "eyebrow", "Live Polling"),
                             tags$h1(class = "hero-title", "Every voice ", tags$em("counts."))
                    ),
                    uiOutput("hero_status_ui")
           ),
           
           conditionalPanel("input.user_role == 'Guest'",
                            
                            conditionalPanel("!output.validGuestConf",
                                             tags$div(class = "pcard",
                                                      tags$div(class = "pcard-title", "Join a session"),
                                                      tags$div(class = "pcard-sub", "Enter the token your organizer shared with you."),
                                                      tags$ul(class = "steps",
                                                              tags$li(tags$span(class="step-n","1"), "Get the 4-character token from your organizer"),
                                                              tags$li(tags$span(class="step-n","2"), "Optionally enter your name, others will see it"),
                                                              tags$li(tags$span(class="step-n","3"), "Vote, suggest answers, and watch results live")
                                                      ),
                                                      tags$div(class = "admin-grid",
                                                               tags$div(
                                                                 tags$div(class = "token-input",
                                                                          textInput("guest_token_input", "Access Token", placeholder = ""))
                                                               ),
                                                               tags$div(
                                                                 textInput("guest_name_input", "Your name (optional)", placeholder = ""),
                                                                 actionButton("guest_enter", "Join Room", class = "btn btn-sage")
                                                               )
                                                      )
                                             )
                            ),
                            
                            conditionalPanel("output.validGuestConf",
                                             uiOutput("room_closed_ui"),
                                             conditionalPanel("!output.isRoomClosed",
                                                              uiOutput("kicked_ui"),
                                                              conditionalPanel("!output.isKicked",
                                                                               
                                                                               tags$div(class = "pcard",
                                                                                        tags$div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:.9rem;flex-wrap:wrap;gap:.5rem;",
                                                                                                 uiOutput("guest_name_badge_ui"),
                                                                                                 uiOutput("user_count_ui_guest")
                                                                                        ),
                                                                                        tags$div(class = "self-rename-row",
                                                                                                 textInput("self_rename_input", NULL, placeholder = "Change your display name..."),
                                                                                                 actionButton("self_rename_btn", "Update", class = "btn btn-ghost btn-sm",
                                                                                                              style = "margin-bottom:1rem;")
                                                                                        ),
                                                                                        tags$div(class = "div"),
                                                                                        tags$div(class = "q-header",
                                                                                                 tags$div(class = "sec-eye", "Active Question"),
                                                                                                 uiOutput("question_ui_guest")
                                                                                        ),
                                                                                        tags$div(style="display:flex;gap:.5rem;flex-wrap:wrap;align-items:center;margin-bottom:.6rem;",
                                                                                                 uiOutput("guest_mode_badges_ui")
                                                                                        ),
                                                                                        uiOutput("guest_timer_ui"),
                                                                                        uiOutput("guest_vote_budget_ui"),
                                                                                        uiOutput("guest_suggest_lock_ui"),
                                                                                        tags$div(id = "answer_input_wrap",
                                                                                                 tags$div(class="sec-eye", "Suggest an answer"),
                                                                                                 tags$div(style="display:flex;gap:.6rem;align-items:flex-end;",
                                                                                                          tags$div(style="flex:1;",
                                                                                                                   textInput("answer_text", NULL, placeholder="Type your answer...")),
                                                                                                          actionButton("submit_answer", "Submit", class="btn btn-sage",
                                                                                                                       style="margin-bottom:1rem;flex-shrink:0;")
                                                                                                 )
                                                                                        ),
                                                                                        uiOutput("guest_answer_section")
                                                                               ),
                                                                               
                                                                               tags$div(class = "pcard",
                                                                                        tags$div(class = "sec-eye", "Activity"),
                                                                                        tags$div(class = "pcard-title", style="margin-bottom:.65rem;", "What's happening"),
                                                                                        uiOutput("activity_feed_guest")
                                                                               )
                                                              )
                                             )
                            )
           ),
           
           conditionalPanel("input.user_role == 'Admin'",
                            
                            conditionalPanel("!output.isAdmin",
                                             tags$div(class = "pcard",
                                                      tags$div(class = "pcard-title", "Create or resume a room"),
                                                      tags$div(class = "pcard-sub", "Start fresh or re-enter your admin key to resume."),
                                                      tags$div(class = "admin-grid",
                                                               tags$div(
                                                                 tags$div(class = "sec-eye", "New room"),
                                                                 tags$ul(class = "steps",
                                                                         tags$li(tags$span(class="step-n","1"), "Enter a conference ID"),
                                                                         tags$li(tags$span(class="step-n","2"), "Share the token with attendees"),
                                                                         tags$li(tags$span(class="step-n","3"), "Start polling")
                                                                 ),
                                                                 textInput("new_conf_id", "Conference ID", placeholder = "e.g. SciConf2025"),
                                                                 actionButton("create_conf", "Create Room", class = "btn btn-sage")
                                                               ),
                                                               tags$div(
                                                                 tags$div(class = "sec-eye", "Resume existing room"),
                                                                 tags$br(),
                                                                 tags$p(style="font-size:.88rem;color:var(--ink-light);margin-bottom:1rem;",
                                                                        "Paste your Admin Key to reclaim your room after a page reload."),
                                                                 textInput("resume_key_input", "Admin Key", placeholder = "Paste your admin key"),
                                                                 actionButton("resume_conf", "Resume Room", class = "btn btn-ghost")
                                                               )
                                                      )
                                             )
                            ),
                            
                            conditionalPanel("output.isAdmin",
                                             uiOutput("admin_token_display"),
                                             uiOutput("user_count_ui"),
                                             
                                             tags$div(class = "admin-tabs",
                                                      tags$button(class = "atab active", "data-tab" = "settings",
                                                                  onclick = "showTab('settings')", "Settings"),
                                                      tags$button(class = "atab", "data-tab" = "questions",
                                                                  onclick = "showTab('questions')", "Questions"),
                                                      tags$button(class = "atab", "data-tab" = "room",
                                                                  onclick = "showTab('room')", "Room"),
                                                      tags$button(class = "atab", "data-tab" = "guests",
                                                                  onclick = "showTab('guests')", "Guests"),
                                                      tags$button(class = "atab", "data-tab" = "export",
                                                                  onclick = "showTab('export')", "Export")
                                             ),
                                             
                                             tags$div(id = "tab-room", class = "tab-panel",
                                                      
                                                      tags$div(class = "pcard",
                                                               tags$div(class = "inline-section",
                                                                        tags$div(
                                                                          tags$div(class = "sec-eye", "Active Question"),
                                                                          uiOutput("question_ui_admin")
                                                                        ),
                                                                        tags$div(style="display:flex;gap:.5rem;align-items:flex-end;flex-wrap:wrap;",
                                                                                 actionButton("force_question", "Send to all", class = "btn btn-ink btn-sm"),
                                                                                 actionButton("release_force",  "Release",     class = "btn btn-ghost btn-sm")
                                                                        )
                                                               ),
                                                               tags$div(class = "div"),
                                                               tags$div(class = "admin-grid",
                                                                        tags$div(
                                                                          tags$div(class = "sec-eye", "Add answer"),
                                                                          tags$div(style="display:flex;gap:.6rem;align-items:flex-end;",
                                                                                   tags$div(style="flex:1;",
                                                                                            textInput("admin_answer_text", label = NULL,
                                                                                                      placeholder = "Add an answer for this question...")),
                                                                                   actionButton("admin_submit_answer", "Add", class = "btn btn-ink",
                                                                                                style = "margin-bottom:1rem;flex-shrink:0;")
                                                                          )
                                                                        ),
                                                                        tags$div(
                                                                          tags$div(class = "sec-eye", "Guest suggestions"),
                                                                          uiOutput("lock_toggle_ui"),
                                                                          tags$p(style="font-size:.78rem;color:var(--ink-light);margin-top:.4rem;",
                                                                                 "When locked, guests can vote but cannot add new answers.")
                                                                        )
                                                               ),
                                                               tags$div(class = "div"),
                                                               tags$div(class = "sec-eye", "Timer"),
                                                               tags$div(style="display:flex;gap:.6rem;align-items:flex-end;flex-wrap:wrap;margin-bottom:.6rem;",
                                                                        numericInput("timer_duration", label = NULL, value = 60, min = 5, max = 3600, width = "80px"),
                                                                        tags$span(style="font-size:.84rem;color:var(--ink-light);margin-bottom:1rem;", "seconds"),
                                                                        tags$div(style="display:flex;gap:.4rem;margin-bottom:1rem;",
                                                                                 actionButton("timer_start", "Start", class = "btn btn-sage btn-sm"),
                                                                                 actionButton("timer_stop",  "Stop",  class = "btn btn-ghost btn-sm"),
                                                                                 actionButton("timer_reset", "Reset", class = "btn btn-ghost btn-sm")
                                                                        )
                                                               ),
                                                               uiOutput("admin_timer_ui"),
                                                               tags$div(class = "div"),
                                                               DTOutput("admin_table"),
                                                               tags$p(style="font-size:.75rem;color:var(--ink-light);margin-top:.5rem;",
                                                                      "Delete an answer. Votes are automatically refunded to guests.")
                                                      ),
                                                      
                                                      tags$div(class = "pcard",
                                                               tags$div(class = "sec-eye", "Activity"),
                                                               tags$div(class = "pcard-title", style="margin-bottom:.65rem;", "Room activity"),
                                                               uiOutput("activity_feed_admin")
                                                      )
                                             ),
                                             
                                             tags$div(id = "tab-questions", class = "tab-panel",
                                                      tags$div(class = "admin-grid",
                                                               tags$div(class = "pcard",
                                                                        tags$div(class = "sec-eye", "Add"),
                                                                        tags$div(class = "pcard-title", "New question"),
                                                                        tags$div(class = "div"),
                                                                        textInput("new_question_text", "Question text", placeholder = "Ask something..."),
                                                                        actionButton("add_question", "Add Question", class = "btn btn-sage")
                                                               ),
                                                               tags$div(class = "pcard",
                                                                        tags$div(class = "sec-eye", "Manage"),
                                                                        tags$div(class = "pcard-title", "Edit or reset"),
                                                                        tags$div(class = "div"),
                                                                        selectInput("delete_question_id", "Delete question", choices = NULL),
                                                                        actionButton("delete_question", "Delete", class = "btn btn-ghost"),
                                                                        tags$div(class = "div"),
                                                                        selectInput("restart_question_id", "Reset responses for", choices = NULL),
                                                                        actionButton("restart_question", "Reset responses", class = "btn btn-ghost")
                                                               )
                                                      ),
                                                      tags$div(class = "pcard",
                                                               tags$div(class = "sec-eye", "Summary"),
                                                               tags$div(class = "pcard-title", "Top answers across all questions"),
                                                               tags$div(style="display:flex;align-items:center;gap:.8rem;margin:.65rem 0 1rem;",
                                                                        tags$span(style="font-size:.88rem;color:var(--ink-light);","Show top"),
                                                                        numericInput("top_n", label = NULL, value = 5, min = 1, max = 50, width = "80px"),
                                                                        tags$span(style="font-size:.88rem;color:var(--ink-light);","answers")
                                                               ),
                                                               uiOutput("top_answers_ui")
                                                      )
                                             ),
                                             
                                             tags$div(id = "tab-settings", class = "tab-panel active",
                                                      tags$div(class = "pcard",
                                                               tags$div(class = "pcard-title", "Session settings"),
                                                               tags$p(style="font-size:.84rem;color:var(--sage-d);margin-bottom:1rem;",
                                                                      "Changes apply instantly to all guests."),
                                                               tags$div(class = "admin-grid",
                                                                        tags$div(
                                                                          tags$div(class = "sec-eye", "Vote limits"),
                                                                          tags$div(class = "div"),
                                                                          uiOutput("setting_total_ui"),
                                                                          tags$br(),
                                                                          uiOutput("setting_per_q_ui")
                                                                        ),
                                                                        tags$div(
                                                                          tags$div(class = "sec-eye", "Answer submission limits"),
                                                                          tags$div(class = "div"),
                                                                          uiOutput("setting_ans_per_q_ui"),
                                                                          tags$br(),
                                                                          uiOutput("setting_ans_total_ui")
                                                                        )
                                                               ),
                                                               tags$div(class = "div"),
                                                               tags$div(class = "sec-eye", "Privacy"),
                                                               tags$div(style="display:flex;flex-direction:column;gap:.5rem;margin-top:.4rem;",
                                                                        checkboxInput("setting_anonymous",
                                                                                      tags$span("Anonymous mode: hide names in guest activity feed"),
                                                                                      value = FALSE),
                                                                        checkboxInput("setting_show_results",
                                                                                      tags$span("Show live results to guests: guests see vote counts and bars"),
                                                                                      value = TRUE)
                                                               )
                                                      )
                                             ),
                                             
                                             tags$div(id = "tab-guests", class = "tab-panel",
                                                      tags$div(class = "pcard",
                                                               tags$div(class = "pcard-title", style="margin-bottom:.75rem;", "Manage guests"),
                                                               uiOutput("guest_mgmt_ui")
                                                      )
                                             ),
                                             
                                             tags$div(id = "tab-export", class = "tab-panel",
                                                      tags$div(class = "pcard",
                                                               tags$div(class = "pcard-title", "Export session data"),
                                                               tags$p(style="font-size:.88rem;color:var(--ink-light);margin:.5rem 0 1.2rem;",
                                                                      "Download all questions and responses as a CSV file."),
                                                               downloadButton("export_csv", "Download CSV", class = "btn btn-ink")
                                                      )
                                             )
                            )
           )
  ),
  
  tags$div(class = "chorus-foot",
           tags$p("Chorus by ",
                  tags$a("Dr. Cristian Román-Palacios · Data Diversity Lab, U of A",
                         href = "https://datadiversitylab.github.io/", target = "_blank"),
                  "  ", tags$a("GitHub",
                               href = "https://github.com/datadiversitylab/chorus", target = "_blank")
           ),
           tags$p("Concept by ",
                  tags$a("Dr. Heidi Steiner", href = "https://heidiesteiner.netlify.app/", target = "_blank")),
           tags$div(class = "foot-note", "\u2726 No data stored permanently unless you export before closing")
  )
)

server <- function(input, output, session) {
  
  sid <- paste0("s_", as.integer(Sys.time()), "_", sample(1e6, 1))
  
  is_admin      <- reactiveVal(FALSE)
  admin_cid     <- reactiveVal(NULL)
  guest_cid     <- reactiveVal(NULL)
  guest_qsel    <- reactiveVal(NULL)
  guest_name    <- reactiveVal("")
  vote_msg      <- reactiveVal("")
  is_kicked     <- reactiveVal(FALSE)
  is_room_closed <- reactiveVal(FALSE)
  rename_target <- reactiveVal(NULL)
  
  output$isAdmin        <- reactive({ is_admin() })
  output$validGuestConf <- reactive({ !is.null(guest_cid()) })
  output$isKicked       <- reactive({ is_kicked() })
  output$isRoomClosed   <- reactive({ is_room_closed() })
  outputOptions(output, "isAdmin",        suspendWhenHidden = FALSE)
  outputOptions(output, "validGuestConf", suspendWhenHidden = FALSE)
  outputOptions(output, "isKicked",       suspendWhenHidden = FALSE)
  outputOptions(output, "isRoomClosed",   suspendWhenHidden = FALSE)
  
  trig_g <- reactive({ cid <- guest_cid(); req(cid); get_trigger(cid)() })
  trig_a <- reactive({ cid <- admin_cid(); req(cid); get_trigger(cid)() })
  
  observe({
    invalidateLater(1500, session)
    cid <- guest_cid(); if (is.null(cid)) return()
    if (isTRUE(get_kick_trig(sid)())) { is_kicked(TRUE); return() }
    e <- get_conf(cid)
    if (is.null(e) || isTRUE(e$room_closed)) { is_room_closed(TRUE); return() }
    gl <- list_guests(cid)
    if (!is.null(gl[[sid]])) {
      stored_nm <- gl[[sid]]$name
      if (stored_nm != guest_name()) guest_name(stored_nm)
    }
    if (is.null(e)) return()
    fq <- e$forced_qid
    if (!is.null(fq) && !is.na(fq) && nchar(fq) > 0) {
      if (is.null(guest_qsel()) || guest_qsel() != fq) {
        guest_qsel(fq)
        updateSelectInput(session, "question_id_guest", selected = fq)
      }
    }
  })
  
  output$kicked_ui <- renderUI({
    if (!is_kicked()) return(NULL)
    tags$div(class = "kicked-screen",
             tags$div(class = "kicked-title", "You've been removed"),
             tags$div(class = "kicked-sub",
                      "The organizer has ended your session. Thank you for participating in Chorus.")
    )
  })
  
  output$room_closed_ui <- renderUI({
    if (!is_room_closed()) return(NULL)
    tags$div(class = "closed-screen",
             tags$div(class = "closed-title", "This room has been closed"),
             tags$div(class = "closed-sub",
                      "The organizer has ended the session. Thank you for participating in Chorus.")
    )
  })
  
  observeEvent(input$close_room, {
    cid <- admin_cid(); req(cid)
    e   <- get_conf(cid); req(e)
    
    gl <- list_guests(cid)
    for (gsid in names(gl)) get_kick_trig(gsid)(TRUE)
    
    token <- names(Filter(function(v) v == cid, as.list(store$tokens)))[1]
    if (!is.null(token) && !is.na(token))
      rm(list = token, envir = store$tokens)
    
    admin_key <- e$admin_key
    if (exists(admin_key, envir = store$admin_keys, inherits = FALSE))
      rm(list = admin_key, envir = store$admin_keys)
    
    all_ledger <- ls(store$vote_ledger)
    prefix <- paste0(cid, "|")
    to_rm  <- all_ledger[startsWith(all_ledger, prefix)]
    if (length(to_rm) > 0) rm(list = to_rm, envir = store$vote_ledger)
    
    if (exists(cid, envir = store$activity_log,  inherits = FALSE)) rm(list = cid, envir = store$activity_log)
    if (exists(cid, envir = store$guests,         inherits = FALSE)) rm(list = cid, envir = store$guests)
    if (exists(cid, envir = store$user_counts,    inherits = FALSE)) rm(list = cid, envir = store$user_counts)
    if (exists(cid, envir = store$conf_triggers,  inherits = FALSE)) rm(list = cid, envir = store$conf_triggers)
    if (exists(cid, envir = store$conferences,    inherits = FALSE)) rm(list = cid, envir = store$conferences)
    
    is_admin(FALSE); admin_cid(NULL)
    showNotification("Room closed and all data removed.")
  })
  
  observeEvent(input$create_conf, {
    id <- trimws(input$new_conf_id)
    if (nchar(id) == 0 || exists(id, envir = store$conferences, inherits = FALSE)) {
      showNotification("Invalid or duplicate conference ID.", type = "error"); return()
    }
    admin_key <- paste0(sample(c(LETTERS, letters, 0:9), 12, replace = TRUE), collapse = "")
    token     <- paste0(sample(c(LETTERS, 0:9), 4, replace = TRUE), collapse = "")
    init_conf(id, admin_key)
    assign(token, id, envir = store$tokens)
    is_admin(TRUE); admin_cid(id)
    updateTextInput(session, "new_conf_id", value = "")
    showNotification(paste("Room created:", id))
  })
  
  observeEvent(input$resume_conf, {
    key <- trimws(input$resume_key_input)
    if (!exists(key, envir = store$admin_keys, inherits = FALSE)) {
      showNotification("Admin key not found. Room may have expired.", type = "error"); return()
    }
    cid <- get(key, envir = store$admin_keys, inherits = FALSE)
    is_admin(TRUE); admin_cid(cid)
    bump(cid); showNotification(paste("Resumed room:", cid))
  })
  
  output$admin_token_display <- renderUI({
    req(admin_cid())
    cid   <- admin_cid(); e <- get_conf(cid); req(e)
    token <- names(Filter(function(v) v == cid, as.list(store$tokens)))[1]
    tags$div(class = "token-strip",
             tags$div(class = "ts-left",
                      tags$div(class = "ts-token-block",
                               tags$div(class = "ts-room-lbl", "Share with attendees"),
                               tags$div(class = "ts-token-val", token %||% ""),
                               tags$div(class = "ts-hint", paste0("Room: ", cid))
                      ),
                      tags$div(class = "ts-key-block",
                               tags$div(class = "ts-key-lbl", "Admin Key  save to resume"),
                               tags$div(class = "ts-key-val", e$admin_key),
                               tags$div(class = "ts-key-hint", "Keep private")
                      )
             ),
             tags$div(class = "ts-right",
                      actionButton("close_room", "Close Room", class = "btn btn-close-room btn-sm")
             )
    )
  })
  
  observeEvent(input$guest_enter, {
    token <- trimws(input$guest_token_input)
    nm    <- trimws(input$guest_name_input)
    if (nchar(token) == 0) {
      showNotification("Please enter an access token.", type = "error"); return()
    }
    if (!exists(token, envir = store$tokens, inherits = FALSE)) {
      showNotification("Invalid token. Check with your organizer.", type = "error"); return()
    }
    cid  <- get(token, envir = store$tokens, inherits = FALSE)
    disp <- if (nchar(nm) > 0) nm else paste0("Guest-", substr(sid, nchar(sid)-3, nchar(sid)))
    guest_cid(cid); guest_name(disp)
    cur <- get(cid, envir = store$user_counts, inherits = FALSE)
    assign(cid, cur + 1L, envir = store$user_counts)
    register_guest(cid, sid, disp)
    updateTextInput(session, "self_rename_input", value = disp)
    add_activity(cid, paste0("<strong>", disp, "</strong> joined the room"),
                 msg_anon = "A new participant joined the room")
    bump(cid)
    lc <- cid; ln <- disp
    session$onSessionEnded(function() {
      if (exists(lc, envir = store$conferences, inherits = FALSE)) {
        remove_guest(lc, sid)
        add_activity(lc, paste0("<strong>", ln, "</strong> left the room"))
        bump(lc)
      }
    })
  })
  
  observeEvent(input$self_rename_btn, {
    cid <- guest_cid(); req(cid)
    nm  <- trimws(input$self_rename_input)
    if (nchar(nm) == 0) { showNotification("Name cannot be empty.", type="error"); return() }
    old <- guest_name()
    guest_name(nm)
    update_guest_name_global(cid, sid, nm)
    add_activity(cid, paste0("<strong>", old, "</strong> is now <strong>", nm, "</strong>"),
                 msg_anon = "A participant updated their name")
    bump(cid); showNotification(paste("Name updated to:", nm))
  })
  
  output$user_count_ui <- renderUI({
    trig_a(); cid <- admin_cid(); if (is.null(cid)) return(NULL)
    n <- get(cid, envir = store$user_counts, inherits = FALSE) %||% 0L
    tags$div(class = "status-bar", tags$div(class = "status-dot"),
             paste0(n, " attendee", if (n!=1)"s" else "", " online in ", cid))
  })
  output$user_count_ui_guest <- renderUI({
    trig_g(); cid <- guest_cid(); if (is.null(cid)) return(NULL)
    n <- get(cid, envir = store$user_counts, inherits = FALSE) %||% 0L
    tags$div(class = "status-bar", tags$div(class = "status-dot"),
             paste0(n, " attendee", if (n!=1)"s" else "", " in this room"))
  })
  output$hero_status_ui <- renderUI({
    if (is_admin()) {
      cid <- admin_cid(); if (is.null(cid)) return(NULL)
      n <- get(cid, envir = store$user_counts, inherits = FALSE) %||% 0L
      tags$div(class = "status-bar", tags$div(class = "status-dot"),
               paste0(n, " online"))
    } else if (!is.null(guest_cid())) {
      n <- get(guest_cid(), envir = store$user_counts, inherits = FALSE) %||% 0L
      tags$div(class = "status-bar", tags$div(class = "status-dot"),
               paste0(n, " in room"))
    } else NULL
  })
  output$guest_name_badge_ui <- renderUI({
    nm <- guest_name(); if (nchar(nm) == 0) return(NULL)
    tags$div(class = "name-badge",
             tags$span(class = "name-badge-dot", "●"), paste0("Participating as: ", nm))
  })
  
  output$guest_mode_badges_ui <- renderUI({
    trig_g(); cid <- guest_cid(); if (is.null(cid)) return(NULL)
    e <- get_conf(cid); if (is.null(e)) return(NULL)
    s <- e$settings
    badges <- list()
    if (isTRUE(s$anonymous))
      badges <- c(badges, list(tags$span(class="mode-badge anon", "🔒 Anonymous")))
    if (!isTRUE(s$show_results))
      badges <- c(badges, list(tags$span(class="mode-badge blind", "👁 Results hidden")))
    if (length(badges) == 0) return(NULL)
    tags$div(style="display:flex;gap:.5rem;flex-wrap:wrap;margin:.5rem 0;", badges)
  })
  
  output$guest_timer_ui <- renderUI({
    invalidateLater(1000, session)
    cid <- guest_cid(); qid <- guest_qsel(); req(cid, qid)
    e <- get_conf(cid); if (is.null(e)) return(NULL)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
    q <- get(qid, envir = e$questions, inherits = FALSE)
    make_timer_ui(q, admin = FALSE)
  })
  
  output$guest_suggest_lock_ui <- renderUI({
    trig_g()
    cid <- guest_cid(); qid <- guest_qsel(); req(cid, qid)
    e <- get_conf(cid); if (is.null(e)) return(NULL)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
    q <- get(qid, envir = e$questions, inherits = FALSE)
    if (isTRUE(q$locked)) {
      tagList(
        tags$div(style="margin-bottom:1rem;",
                 tags$div(class="lock-banner",
                          tags$span(class="lock-icon","🔒"),
                          "The organizer has locked new suggestions for this question.")),
        tags$script(HTML("document.getElementById('answer_input_wrap').style.display='none';"))
      )
    } else {
      tags$script(HTML("document.getElementById('answer_input_wrap').style.display='';"))
    }
  })
  
  
  
  output$setting_total_ui <- renderUI({
    trig_a(); cid <- admin_cid(); req(cid)
    e <- get_conf(cid); req(e); cur <- e$settings$max_votes_total
    tagList(
      tags$div(class="sec-eye","Max votes per attendee (all questions)"),
      tags$div(class="limit-row",
               checkboxInput("total_unlimited","Unlimited", value=is.infinite(cur)),
               if (!is.infinite(cur))
                 numericInput("max_votes_total", label=NULL, value=cur, min=1, width="90px")
               else
                 tags$span(class="limit-badge unlimited","∞  No limit")
      )
    )
  })
  
  output$setting_per_q_ui <- renderUI({
    trig_a(); cid <- admin_cid(); req(cid)
    e <- get_conf(cid); req(e); cur <- e$settings$max_votes_per_q
    tagList(
      tags$div(class="sec-eye","Max votes per attendee per question"),
      tags$div(class="limit-row",
               checkboxInput("per_q_unlimited","Unlimited", value=is.infinite(cur)),
               if (!is.infinite(cur))
                 numericInput("max_votes_per_q", label=NULL, value=cur, min=1, width="90px")
               else
                 tags$span(class="limit-badge unlimited","∞  No limit")
      )
    )
  })
  
  observeEvent(input$total_unlimited, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    e$settings$max_votes_total <- if (isTRUE(input$total_unlimited)) Inf
    else max(1L, as.integer(input$max_votes_total %||% 5L))
    bump(cid)
  }, ignoreInit = TRUE)
  
  observeEvent(input$max_votes_total, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    if (!isTRUE(input$total_unlimited) && !is.null(input$max_votes_total)) {
      e$settings$max_votes_total <- max(1L, as.integer(input$max_votes_total))
      bump(cid)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$per_q_unlimited, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    e$settings$max_votes_per_q <- if (isTRUE(input$per_q_unlimited)) Inf
    else max(1L, as.integer(input$max_votes_per_q %||% 1L))
    bump(cid)
  }, ignoreInit = TRUE)
  
  observeEvent(input$max_votes_per_q, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    if (!isTRUE(input$per_q_unlimited) && !is.null(input$max_votes_per_q)) {
      e$settings$max_votes_per_q <- max(1L, as.integer(input$max_votes_per_q))
      bump(cid)
    }
  }, ignoreInit = TRUE)
  
  output$setting_ans_per_q_ui <- renderUI({
    trig_a(); cid <- admin_cid(); req(cid)
    e <- get_conf(cid); req(e); cur <- e$settings$max_answers_per_q
    tagList(
      tags$div(class="sec-eye","Max answers a guest can suggest per question"),
      tags$div(class="limit-row",
               checkboxInput("ans_per_q_unlimited","Unlimited", value=is.infinite(cur)),
               if (!is.infinite(cur))
                 numericInput("max_answers_per_q", label=NULL, value=cur, min=1, width="90px")
               else
                 tags$span(class="limit-badge unlimited","\u221e  No limit")
      )
    )
  })
  
  output$setting_ans_total_ui <- renderUI({
    trig_a(); cid <- admin_cid(); req(cid)
    e <- get_conf(cid); req(e); cur <- e$settings$max_answers_total
    tagList(
      tags$div(class="sec-eye","Max answers a guest can suggest across all questions"),
      tags$div(class="limit-row",
               checkboxInput("ans_total_unlimited","Unlimited", value=is.infinite(cur)),
               if (!is.infinite(cur))
                 numericInput("max_answers_total", label=NULL, value=cur, min=1, width="90px")
               else
                 tags$span(class="limit-badge unlimited","\u221e  No limit")
      )
    )
  })
  
  observeEvent(input$ans_per_q_unlimited, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    e$settings$max_answers_per_q <- if (isTRUE(input$ans_per_q_unlimited)) Inf
    else max(1L, as.integer(input$max_answers_per_q %||% 1L))
    bump(cid)
  }, ignoreInit = TRUE)
  
  observeEvent(input$max_answers_per_q, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    if (!isTRUE(input$ans_per_q_unlimited) && !is.null(input$max_answers_per_q)) {
      e$settings$max_answers_per_q <- max(1L, as.integer(input$max_answers_per_q))
      bump(cid)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$ans_total_unlimited, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    e$settings$max_answers_total <- if (isTRUE(input$ans_total_unlimited)) Inf
    else max(1L, as.integer(input$max_answers_total %||% 5L))
    bump(cid)
  }, ignoreInit = TRUE)
  
  observeEvent(input$max_answers_total, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    if (!isTRUE(input$ans_total_unlimited) && !is.null(input$max_answers_total)) {
      e$settings$max_answers_total <- max(1L, as.integer(input$max_answers_total))
      bump(cid)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$setting_anonymous, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    e$settings$anonymous <- isTRUE(input$setting_anonymous)
    bump(cid)
  }, ignoreInit = TRUE)
  
  observeEvent(input$setting_show_results, {
    cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
    e$settings$show_results <- isTRUE(input$setting_show_results)
    bump(cid)
  }, ignoreInit = TRUE)
  
  observeEvent(input$timer_start, {
    cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e   <- get_conf(cid)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return()
    q   <- get(qid, envir = e$questions, inherits = FALSE)
    dur <- max(5L, as.integer(input$timer_duration %||% 60L))
    q$timer_secs   <- dur
    q$timer_end    <- Sys.time() + dur
    q$timer_active <- TRUE
    assign(qid, q, envir = e$questions)
    add_activity(cid, paste0("Timer started: <strong>", dur, "s</strong> for <em>", q$text, "</em>"),
                 admin_only = TRUE)
    bump(cid)
  })
  observeEvent(input$timer_stop, {
    cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e   <- get_conf(cid)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return()
    q   <- get(qid, envir = e$questions, inherits = FALSE)
    q$timer_active <- FALSE; q$timer_end <- NULL
    assign(qid, q, envir = e$questions); bump(cid)
  })
  observeEvent(input$timer_reset, {
    cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e   <- get_conf(cid)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return()
    q   <- get(qid, envir = e$questions, inherits = FALSE)
    q$timer_active <- FALSE; q$timer_end <- NULL; q$timer_secs <- 0L
    assign(qid, q, envir = e$questions); bump(cid)
  })
  
  observe({
    invalidateLater(1000, session)
    cid <- admin_cid(); if (is.null(cid)) return()
    e   <- get_conf(cid); if (is.null(e)) return()
    qids <- ls(e$questions)
    for (qid in qids) {
      q <- get(qid, envir = e$questions, inherits = FALSE)
      if (isTRUE(q$timer_active) && !is.null(q$timer_end)) {
        remaining <- as.numeric(difftime(q$timer_end, Sys.time(), units = "secs"))
        if (remaining <= 0) {
          q$timer_active <- FALSE
          q$timer_end    <- NULL
          q$locked       <- TRUE
          assign(qid, q, envir = e$questions)
          add_activity(cid,
                       paste0("Timer expired for <strong>", q$text, "</strong>. Suggestions locked."),
                       admin_only = TRUE)
          bump(cid)
        }
      }
    }
  })
  
  output$admin_timer_ui <- renderUI({
    invalidateLater(1000, session)
    trig_a()
    cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e <- get_conf(cid); req(e)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
    q <- get(qid, envir = e$questions, inherits = FALSE)
    make_timer_ui(q, admin = TRUE)
  })
  
  
  observeEvent(input$force_question, {
    cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e   <- get_conf(cid); req(e)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return()
    q   <- get(qid, envir = e$questions, inherits = FALSE)
    e$forced_qid <- qid
    add_activity(cid, paste0("All guests navigated to: <strong>", q$text, "</strong>"), admin_only = TRUE)
    bump(cid); showNotification("All guests moved to that question.")
  })
  observeEvent(input$release_force, {
    cid <- admin_cid(); req(cid)
    e   <- get_conf(cid); req(e)
    e$forced_qid <- NULL
    bump(cid); showNotification("Question navigation released.")
  })
  
  qs_a <- reactive({
    trig_a(); cid <- admin_cid(); req(cid)
    e <- get_conf(cid); req(e); as.list(e$questions)
  })
  qs_g <- reactive({
    trig_g(); cid <- guest_cid(); req(cid)
    e <- get_conf(cid); req(e); as.list(e$questions)
  })
  
  observe({
    qs <- qs_a()
    if (length(qs) == 0) {
      updateSelectInput(session, "delete_question_id",  choices = character(0))
      updateSelectInput(session, "restart_question_id", choices = character(0)); return()
    }
    ch <- setNames(names(qs), sapply(qs, `[[`, "text"))
    updateSelectInput(session, "delete_question_id",  choices = ch)
    updateSelectInput(session, "restart_question_id", choices = ch)
  })
  
  observeEvent(input$add_question, {
    req(admin_cid(), nchar(trimws(input$new_question_text)) > 0)
    cid <- admin_cid(); e <- get_conf(cid)
    qid <- paste0("q", as.integer(Sys.time()), "_", sample(1000, 1))
    txt <- trimws(input$new_question_text)
    assign(qid, list(
      text         = txt,
      locked       = FALSE,
      timer_secs   = 0L,
      timer_end    = NULL,
      timer_active = FALSE,
      responses = data.frame(ID=integer(), Answer=character(), Votes=integer(),
                             Submitter=character(), IsAdmin=logical(),
                             stringsAsFactors=FALSE)
    ), envir = e$questions)
    updateTextInput(session, "new_question_text", value = "")
    add_activity(cid, paste0("Question added: <strong>", txt, "</strong>"), admin_only = TRUE)
    bump(cid); showNotification("Question added.")
  })
  
  observeEvent(input$delete_question, {
    req(admin_cid(), input$delete_question_id)
    cid <- admin_cid()
    rm(list = input$delete_question_id, envir = get_conf(cid)$questions)
    bump(cid); showNotification("Question deleted.", type = "warning")
  })
  
  observeEvent(input$restart_question, {
    req(admin_cid(), input$restart_question_id)
    cid <- admin_cid(); qid <- input$restart_question_id
    q <- get(qid, envir = get_conf(cid)$questions, inherits = FALSE)
    q$responses <- data.frame(ID=integer(), Answer=character(), Votes=integer(),
                              Submitter=character(), IsAdmin=logical(),
                              stringsAsFactors=FALSE)
    assign(qid, q, envir = get_conf(cid)$questions)
    all_keys <- ls(store$vote_ledger)
    q_suffix <- paste0("|", qid)
    to_clear <- all_keys[endsWith(all_keys, q_suffix) & startsWith(all_keys, paste0(cid,"|"))]
    for (k in to_clear) assign(k, setNames(integer(),character()), envir=store$vote_ledger)
    bump(cid); showNotification("Responses reset.")
  })
  
  output$question_ui_admin <- renderUI({
    qs <- qs_a()
    if (length(qs) == 0) return(tags$p(style="color:var(--ink-light);font-size:.95rem;","No questions yet."))
    selectInput("question_id_admin","Select question to view:",
                choices = setNames(names(qs), sapply(qs, `[[`, "text")))
  })
  
  output$lock_toggle_ui <- renderUI({
    trig_a(); cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e <- get_conf(cid); req(e)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
    q <- get(qid, envir = e$questions, inherits = FALSE)
    locked <- isTRUE(q$locked)
    if (locked)
      tags$div(
        tags$div(class="lock-banner",
                 tags$span(class="lock-icon","🔒"), "Suggestions locked for this question."),
        actionButton("toggle_lock","Unlock suggestions", class="btn btn-ghost btn-sm")
      )
    else
      actionButton("toggle_lock","🔒 Lock suggestions", class="btn btn-ghost btn-sm")
  })
  
  observeEvent(input$toggle_lock, {
    cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e   <- get_conf(cid)
    q   <- get(qid, envir = e$questions, inherits = FALSE)
    q$locked <- !isTRUE(q$locked)
    assign(qid, q, envir = e$questions)
    add_activity(cid, paste0("Suggestions <strong>",
                             if(q$locked)"locked" else "unlocked","</strong> for: <em>",q$text,"</em>"), admin_only = TRUE)
    bump(cid)
  })
  
  observeEvent(input$admin_submit_answer, {
    cid <- admin_cid(); qid <- input$question_id_admin
    ans <- trimws(input$admin_answer_text); req(cid, qid, nchar(ans) > 0)
    e   <- get_conf(cid)
    q   <- get(qid, envir = e$questions, inherits = FALSE)
    new_id <- if (nrow(q$responses)==0) 1L else max(q$responses$ID) + 1L
    q$responses <- rbind(q$responses,
                         data.frame(ID=new_id, Answer=ans, Votes=0L,
                                    Submitter="Organizer", IsAdmin=TRUE, stringsAsFactors=FALSE))
    assign(qid, q, envir = e$questions)
    updateTextInput(session, "admin_answer_text", value = "")
    add_activity(cid, paste0("Organizer suggested: <em>", ans, "</em>"), admin_only = TRUE)
    bump(cid); showNotification("Answer added.")
  })
  
  observeEvent(input$delete_answer_action, {
    cid <- admin_cid(); req(cid)
    da  <- input$delete_answer_action
    qid <- da$qid; aid_char <- as.character(da$aid); req(qid, aid_char)
    e   <- get_conf(cid)
    q   <- get(qid, envir = e$questions, inherits = FALSE)
    ri  <- which(as.character(q$responses$ID) == aid_char)
    if (length(ri) == 0) return()
    deleted_ans <- q$responses$Answer[ri[1]]
    
    refunds <- refund_answer_votes(cid, qid, aid_char)
    
    q$responses <- q$responses[-ri, , drop = FALSE]
    assign(qid, q, envir = e$questions)
    
    n_refunds <- length(refunds)
    msg <- paste0("Answer deleted: <em>", deleted_ans, "</em>")
    if (n_refunds > 0)
      msg <- paste0(msg, ". <strong>", n_refunds, "</strong> guest",
                    if(n_refunds!=1)"s'" else "'", " vote", if(n_refunds!=1)"s" else "",
                    " refunded")
    add_activity(cid, msg, admin_only = TRUE)
    bump(cid); showNotification(paste0("Answer deleted. ", n_refunds, " vote(s) refunded."), type = "warning")
  })
  
  output$admin_table <- renderDT({
    trig_a(); cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
    e <- get_conf(cid); req(e)
    if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
    r <- get(qid, envir = e$questions, inherits = FALSE)$responses
    if (nrow(r) == 0)
      return(datatable(data.frame(Message="No responses yet"), rownames=FALSE,
                       options=list(dom="t")))
    r_ord   <- r[order(-r$Votes), ]
    del_btns <- sapply(seq_len(nrow(r_ord)), function(i) {
      sprintf(
        '<button class="vbtn vbtn-del" title="Delete answer"
          onclick="Shiny.setInputValue(\'delete_answer_action\',{aid:%d,qid:\'%s\'},{priority:\'event\'})">🗑</button>',
        r_ord$ID[i], qid)
    })
    out <- data.frame(
      Answer = r_ord$Answer,
      Votes  = r_ord$Votes,
      By     = ifelse(isTRUE(r_ord$IsAdmin), "★ Organizer", r_ord$Submitter),
      Delete = del_btns,
      stringsAsFactors = FALSE
    )
    datatable(out, rownames=FALSE, escape=FALSE,
              options=list(dom="tp", pageLength=15,
                           columnDefs=list(list(orderable=FALSE, targets=3))))
  })
  
  output$top_answers_ui <- renderUI({
    trig_a(); cid <- admin_cid(); req(cid)
    e <- get_conf(cid); req(e)
    qs <- as.list(e$questions)
    if (length(qs)==0) return(tags$p(style="color:var(--ink-light);font-size:.95rem;","No questions yet."))
    n_top <- max(1L, as.integer(input$top_n %||% 5L))
    all_rows <- do.call(rbind, lapply(names(qs), function(qid) {
      q <- qs[[qid]]; r <- q$responses
      if (nrow(r)==0) return(NULL); r$Question <- q$text; r
    }))
    if (is.null(all_rows)||nrow(all_rows)==0)
      return(tags$p(style="color:var(--ink-light);font-size:.95rem;","No answers yet."))
    top   <- head(all_rows[order(-all_rows$Votes),], n_top)
    max_v <- max(top$Votes, 1)
    rows  <- lapply(seq_len(nrow(top)), function(i) {
      rc  <- if(i==1)"rank-1" else if(i==2)"rank-2" else if(i==3)"rank-3" else "rank-n"
      pct <- round(top$Votes[i]/max_v*100)
      tags$tr(
        tags$td(tags$span(class=paste("rank-badge",rc), i)),
        tags$td(style="font-weight:500;", top$Answer[i]),
        tags$td(style="color:var(--ink-light);font-size:.85rem;max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;", top$Question[i]),
        tags$td(style="font-family:'Fraunces',serif;font-weight:700;text-align:right;", top$Votes[i]),
        tags$td(tags$div(class="mini-bar-wrap",
                         tags$div(class="mini-bar",style=paste0("width:",pct,"%;"))
        ))
      )
    })
    tags$table(class="topans-table",
               tags$thead(tags$tr(tags$th("#"),tags$th("Answer"),tags$th("Question"),
                                  tags$th(style="text-align:right;","Votes"),tags$th("Share"))),
               tags$tbody(rows)
    )
  })
  
  output$guest_mgmt_ui <- renderUI({
    trig_a(); cid <- admin_cid(); req(cid)
    gl <- list_guests(cid)
    if (length(gl)==0)
      return(tags$p(style="color:var(--ink-light);font-size:.92rem;","No guests connected yet."))
    rows <- lapply(names(gl), function(gsid) {
      g <- gl[[gsid]]
      tags$tr(
        tags$td(style="font-weight:500;", g$name),
        tags$td(style="color:var(--ink-light);font-size:.8rem;",
                substr(gsid, nchar(gsid)-5, nchar(gsid))),
        tags$td(
          tags$div(style="display:flex;gap:.5rem;flex-wrap:wrap;",
                   actionButton(paste0("ren_",gsid),"Rename", class="btn btn-ghost btn-sm",
                                onclick=paste0("Shiny.setInputValue('rename_target_sid','",gsid,"',{priority:'event'})")),
                   actionButton(paste0("kck_",gsid),"Remove", class="btn btn-danger btn-sm",
                                onclick=paste0("Shiny.setInputValue('kick_sid','",gsid,"',{priority:'event'})"))
          )
        )
      )
    })
    rename_ui <- if (!is.null(rename_target())) {
      gsid     <- rename_target()
      gl2      <- list_guests(cid)
      cur_name <- if (!is.null(gl2[[gsid]])) gl2[[gsid]]$name else ""
      tags$div(style="margin-top:1rem;padding:1rem;background:var(--sage-mist);border-radius:var(--r-md);border:1px solid var(--sage-pale);",
               tags$div(class="sec-eye","Rename guest"),
               tags$div(style="display:flex;gap:.75rem;align-items:flex-end;",
                        tags$div(style="flex:1;",
                                 textInput("rename_new_name", NULL, value=cur_name, placeholder="New display name")),
                        actionButton("rename_confirm","Save",   class="btn btn-sage btn-sm",  style="margin-bottom:1rem;"),
                        actionButton("rename_cancel", "Cancel", class="btn btn-ghost btn-sm", style="margin-bottom:1rem;")
               )
      )
    } else NULL
    tagList(
      tags$table(class="guest-table",
                 tags$thead(tags$tr(tags$th("Name"),tags$th("Session"),tags$th("Actions"))),
                 tags$tbody(rows)
      ),
      rename_ui
    )
  })
  
  observeEvent(input$rename_target_sid, { rename_target(input$rename_target_sid) })
  observeEvent(input$rename_cancel,     { rename_target(NULL) })
  observeEvent(input$rename_confirm, {
    cid  <- admin_cid(); gsid <- rename_target(); req(cid, gsid)
    nm   <- trimws(input$rename_new_name)
    if (nchar(nm)==0) { showNotification("Name cannot be empty.", type="error"); return() }
    old  <- list_guests(cid)[[gsid]]$name %||% "Guest"
    update_guest_name_global(cid, gsid, nm)
    add_activity(cid, paste0("<strong>",old,"</strong> renamed to <strong>",nm,"</strong>"),
                 msg_anon = "A participant was renamed")
    rename_target(NULL); bump(cid); showNotification(paste("Renamed to:", nm))
  })
  observeEvent(input$kick_sid, {
    cid  <- admin_cid(); gsid <- input$kick_sid; req(cid, gsid)
    nm   <- list_guests(cid)[[gsid]]$name %||% "Guest"
    get_kick_trig(gsid)(TRUE)
    remove_guest(cid, gsid)
    add_activity(cid, paste0("<strong>",nm,"</strong> was removed from the room"),
                 msg_anon = "A participant left the room")
    bump(cid); showNotification(paste("Removed:", nm), type="warning")
  })
  
  output$export_csv <- downloadHandler(
    filename = function() paste0("chorus_", admin_cid()%||%"export","_",format(Sys.Date(),"%Y%m%d"),".csv"),
    content  = function(file) {
      cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e)
      qs  <- as.list(e$questions)
      if (length(qs)==0) { write.csv(data.frame(Message="No data"), file, row.names=FALSE); return() }
      all_rows <- do.call(rbind, lapply(names(qs), function(qid) {
        q <- qs[[qid]]; r <- q$responses
        if (nrow(r)==0) return(NULL)
        data.frame(Conference=cid, Question=q$text, Answer=r$Answer, Votes=r$Votes,
                   Submitter=r$Submitter%||%"",
                   AdminSuggested=if(!is.null(r$IsAdmin)) r$IsAdmin else FALSE,
                   stringsAsFactors=FALSE)
      }))
      if (is.null(all_rows)) all_rows <- data.frame(Message="No responses yet")
      write.csv(all_rows, file, row.names=FALSE)
    }
  )
  
  output$question_ui_guest <- renderUI({
    qs <- qs_g()
    if (length(qs)==0)
      return(tags$p(style="color:var(--ink-light);font-size:.95rem;padding:.5rem 0;",
                    "Waiting for the organizer to post a question."))
    selectInput("question_id_guest", NULL,
                choices = setNames(names(qs), sapply(qs, `[[`, "text")),
                selected = guest_qsel())
  })
  observeEvent(input$question_id_guest, { guest_qsel(input$question_id_guest) })
  
  output$guest_vote_budget_ui <- renderUI({
    trig_g()
    cid <- guest_cid(); qid <- guest_qsel(); req(cid, qid)
    e   <- get_conf(cid); req(e); s <- e$settings
    total_cast <- total_votes_cast(cid, sid)
    q_cast     <- sum(get_votes_for(cid, sid, qid), na.rm=TRUE)
    total_ok   <- within_total_limit(total_cast, s$max_votes_total)
    per_q_ok   <- within_q_limit(q_cast, s$max_votes_per_q)
    total_lbl  <- if (is.infinite(s$max_votes_total)) paste0("Total: ", total_cast, " votes (unlimited)")
    else paste0("Total: ", total_cast, " / ", s$max_votes_total)
    per_q_lbl  <- if (is.infinite(s$max_votes_per_q)) paste0("This question: ", q_cast, " votes (unlimited)")
    else paste0("This question: ", q_cast, " / ", s$max_votes_per_q)
    tags$div(class="vote-budget",
             tags$div(class=paste("budget-pill",if(!total_ok)"warn" else ""),
                      tags$div(class=paste("budget-dot",if(!total_ok)"warn" else "")), total_lbl),
             tags$div(class=paste("budget-pill",if(!per_q_ok)"warn" else ""),
                      tags$div(class=paste("budget-dot",if(!per_q_ok)"warn" else "")), per_q_lbl)
    )
  })
  
  output$guest_answer_section <- renderUI({
    trig_g()
    cid <- guest_cid(); qid <- guest_qsel(); req(cid, qid)
    if (is_kicked()) return(NULL)
    e <- get_conf(cid); req(e)
    if (!exists(qid, envir=e$questions, inherits=FALSE)) return(NULL)
    q            <- get(qid, envir=e$questions, inherits=FALSE)
    responses    <- q$responses
    s            <- e$settings
    locked       <- isTRUE(q$locked)
    show_results <- isTRUE(s$show_results)
    timer_expired <- !isTRUE(q$timer_active) && !is.null(q$timer_secs) && q$timer_secs > 0 &&
      is.null(q$timer_end) && isTRUE(q$locked)
    
    votes_by_user   <- get_votes_for(cid, sid, qid)
    total_cast      <- total_votes_cast(cid, sid)
    q_cast          <- sum(votes_by_user, na.rm=TRUE)
    total_ok        <- within_total_limit(total_cast, s$max_votes_total)
    per_q_ok        <- within_q_limit(q_cast, s$max_votes_per_q)
    at_limit        <- !total_ok || !per_q_ok
    total_all_votes <- if (nrow(responses)>0) sum(responses$Votes) else 0L
    
    if (nrow(responses)==0)
      return(tags$p(style="color:var(--ink-light);font-size:.95rem;",
                    if(locked)"No answers yet." else "No answers yet. Be the first!"))
    
    responses <- responses[order(-responses$Votes),]
    
    cards <- lapply(seq_len(nrow(responses)), function(i) {
      aid       <- as.character(responses$ID[i])
      my_v      <- if (!is.na(votes_by_user[aid])) votes_by_user[aid] else 0L
      has_voted <- my_v > 0L
      pct       <- if (show_results && total_all_votes>0) round(responses$Votes[i]/total_all_votes*100) else 0L
      is_adm    <- isTRUE(responses$IsAdmin[i])
      anon_mode <- isTRUE(s$anonymous)
      card_cls  <- paste0("answer-card",
                          if(has_voted)" voted" else "",
                          if(is_adm)" admin-card" else "",
                          if(!show_results)" blind-mode" else "")
      sub_lbl   <- if (is_adm) "★ Organizer"
      else if (!anon_mode && !is.null(responses$Submitter) &&
               !is.na(responses$Submitter[i]) && nchar(responses$Submitter[i])>0)
        paste0("Suggested by ", responses$Submitter[i]) else ""
      vote_lbl  <- if (show_results)
        paste0(responses$Votes[i]," vote",if(responses$Votes[i]!=1)"s" else "",
               if(pct>0) paste0(" (",pct,"%)") else "")
      else ""
      minus_btn <- if (has_voted)
        actionButton(paste0("m_",aid),"−",class="vbtn vbtn-minus",
                     onclick=paste0("Shiny.setInputValue('vote_action',{aid:'",aid,"',qid:'",qid,"',dir:-1},{priority:'event'})"))
      else NULL
      can_plus  <- !at_limit || has_voted
      plus_btn  <- if (can_plus)
        actionButton(paste0("p_",aid),"+",class="vbtn vbtn-plus",
                     onclick=paste0("Shiny.setInputValue('vote_action',{aid:'",aid,"',qid:'",qid,"',dir:1},{priority:'event'})"))
      else
        tags$button("+",class="vbtn vbtn-plus",disabled=NA,style="opacity:.3;cursor:not-allowed;")
      tags$div(class=card_cls,
               if(show_results && pct>0) tags$div(class="answer-bar-bg",style=paste0("width:",pct,"%;")) else NULL,
               tags$div(class="answer-content",
                        tags$div(class="answer-text", responses$Answer[i],
                                 if(is_adm) tags$span(class="admin-tag","Organizer") else NULL),
                        tags$div(class="answer-meta",
                                 if(nchar(sub_lbl)>0) sub_lbl else "",
                                 if(nchar(sub_lbl)>0 && nchar(vote_lbl)>0)"  " else "",
                                 vote_lbl)
               ),
               tags$div(class="answer-vote-controls", minus_btn,
                        if(show_results)
                          tags$div(class="vote-count", responses$Votes[i])
                        else
                          tags$div(class="my-vote-count", paste0("My votes: ", my_v)),
                        plus_btn)
      )
    })
    
    msg_ui <- if (nchar(vote_msg())>0) tags$div(class="vote-msg", vote_msg()) else NULL
    
    tagList(
      tags$div(class="sec-eye","Vote on answers"),
      tags$div(class="answer-list", cards),
      msg_ui
    )
  })
  
  observeEvent(input$vote_action, {
    if (is_kicked()) return()
    va  <- input$vote_action
    cid <- guest_cid(); req(cid)
    qid <- va$qid; aid_char <- as.character(va$aid); dir <- as.integer(va$dir)
    req(!is.null(qid), !is.null(aid_char))
    e   <- get_conf(cid); req(e)
    q   <- get(qid, envir=e$questions, inherits=FALSE)
    s   <- e$settings
    rs  <- q$responses
    ri  <- which(as.character(rs$ID)==aid_char)
    if (length(ri)==0) return()
    
    votes     <- get_votes_for(cid, sid, qid)
    cur_my    <- if (!is.na(votes[aid_char])) votes[aid_char] else 0L
    total_cast<- total_votes_cast(cid, sid)
    q_cast    <- sum(votes, na.rm=TRUE)
    nm        <- guest_name(); if (nchar(nm)==0) nm <- "Someone"
    
    if (dir==1L && isTRUE(q$locked) && !isTRUE(q$timer_active) &&
        !is.null(q$timer_secs) && q$timer_secs > 0) {
      vote_msg("Time is up. Voting is closed for this question."); return()
    }
    
    if (dir==1L) {
      if (!within_total_limit(total_cast, s$max_votes_total)) {
        vote_msg(paste0("Total vote limit reached (",s$max_votes_total," across all questions)")); return()
      }
      if (!within_q_limit(q_cast, s$max_votes_per_q)) {
        vote_msg(paste0("Vote limit reached for this question (",s$max_votes_per_q," max)")); return()
      }
      rs$Votes[ri]    <- rs$Votes[ri] + 1L
      votes[aid_char] <- cur_my + 1L
      add_activity(cid, paste0("<strong>",nm,"</strong> voted for <em>",rs$Answer[ri],"</em>"),
                   msg_anon = paste0("A new vote was cast for <em>",rs$Answer[ri],"</em>"))
    } else {
      if (cur_my<=0L) return()
      rs$Votes[ri]    <- max(0L, rs$Votes[ri]-1L)
      votes[aid_char] <- cur_my - 1L
      add_activity(cid, paste0("<strong>",nm,"</strong> removed a vote from <em>",rs$Answer[ri],"</em>"),
                   msg_anon = paste0("A vote was removed from <em>",rs$Answer[ri],"</em>"))
    }
    q$responses <- rs
    assign(qid, q, envir=e$questions)
    set_votes_for(cid, sid, qid, votes)
    vote_msg(""); bump(cid)
  })
  
  observeEvent(input$submit_answer, {
    if (is_kicked()) return()
    cid <- guest_cid(); qid <- guest_qsel()
    ans <- trimws(input$answer_text); req(cid, qid, nchar(ans)>0)
    e   <- get_conf(cid)
    q   <- get(qid, envir=e$questions, inherits=FALSE)
    s   <- e$settings
    if (isTRUE(q$locked)) { showNotification("Suggestions are locked.",type="warning"); return() }
    
    ans_this_q <- sum(q$responses$Submitter == (if(nchar(guest_name())>0) guest_name() else "A guest") &
                        !isTRUE(q$responses$IsAdmin), na.rm=TRUE)
    all_keys   <- ls(store$vote_ledger)
    ans_total  <- sum(sapply(ls(e$questions), function(qqid) {
      qq <- get(qqid, envir=e$questions, inherits=FALSE)
      nm_check <- if(nchar(guest_name())>0) guest_name() else "A guest"
      sum(qq$responses$Submitter == nm_check & !isTRUE(qq$responses$IsAdmin), na.rm=TRUE)
    }))
    
    max_apq <- s$max_answers_per_q %||% 1L
    max_at  <- s$max_answers_total %||% Inf
    if (!is.infinite(max_apq) && ans_this_q >= max_apq) {
      showNotification(paste0("Answer limit reached for this question (", max_apq, " max)."), type="warning")
      return()
    }
    if (!is.infinite(max_at) && ans_total >= max_at) {
      showNotification(paste0("Total answer limit reached (", max_at, " across all questions)."), type="warning")
      return()
    }
    
    new_id <- if (nrow(q$responses)==0) 1L else max(q$responses$ID)+1L
    nm <- guest_name(); if (nchar(nm)==0) nm <- "A guest"
    q$responses <- rbind(q$responses,
                         data.frame(ID=new_id, Answer=ans, Votes=0L, Submitter=nm, IsAdmin=FALSE,
                                    stringsAsFactors=FALSE))
    assign(qid, q, envir=e$questions)
    updateTextInput(session,"answer_text",value="")
    session$sendCustomMessage("clearAnswerInput", list())
    add_activity(cid, paste0("<strong>",nm,"</strong> suggested: <em>",ans,"</em>"),
                 msg_anon = paste0("A new answer was suggested: <em>",ans,"</em>"))
    bump(cid)
  })
  
  make_feed <- function(cid, is_guest = FALSE) {
    if (is.null(cid)||!exists(cid,envir=store$activity_log,inherits=FALSE))
      return(tags$div(class="activity-feed",tags$div(class="activity-empty","No activity yet.")))
    log <- get(cid,envir=store$activity_log,inherits=FALSE)
    if (length(log)==0)
      return(tags$div(class="activity-feed",tags$div(class="activity-empty","No activity yet.")))
    anon <- FALSE
    if (is_guest) {
      e <- get_conf(cid)
      if (!is.null(e)) anon <- isTRUE(e$settings$anonymous)
    }
    items <- lapply(log, function(ev) {
      if (is_guest && !isTRUE(ev$admin_only)) return(NULL)
      msg_text <- if (anon && !is.null(ev$msg_anon)) ev$msg_anon
      else if (anon && is.null(ev$msg_anon)) return(NULL)
      else ev$msg
      tags$div(class="activity-item",
               tags$span(class="activity-ts", ev$ts),
               tags$span(class="activity-msg", HTML(msg_text))
      )
    })
    items <- Filter(Negate(is.null), items)
    if (length(items) == 0)
      return(tags$div(class="activity-feed",tags$div(class="activity-empty","No activity yet.")))
    tags$div(class="activity-feed", items)
  }
  output$activity_feed_guest <- renderUI({ trig_g(); make_feed(guest_cid(), is_guest = TRUE) })
  output$activity_feed_admin <- renderUI({ trig_a(); make_feed(admin_cid(), is_guest = FALSE) })
}

shinyApp(ui, server)