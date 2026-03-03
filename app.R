library(shiny)
library(DT)

# ============================================================================
# GLOBAL STORE
# ============================================================================
store <- new.env(parent = emptyenv())
store$conferences   <- new.env(parent = emptyenv())
store$tokens        <- new.env(parent = emptyenv())
store$admin_keys    <- new.env(parent = emptyenv())
store$user_counts   <- new.env(parent = emptyenv())
store$conf_triggers <- new.env(parent = emptyenv())
store$activity_log  <- new.env(parent = emptyenv())
store$guests        <- new.env(parent = emptyenv())
store$kick_triggers <- new.env(parent = emptyenv())

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── trigger helpers ──────────────────────────────────────────────────────────
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

# ── conference helpers ───────────────────────────────────────────────────────
get_conf <- function(cid) {
    if (!exists(cid, envir = store$conferences, inherits = FALSE)) return(NULL)
    get(cid, envir = store$conferences, inherits = FALSE)
}

init_conf <- function(cid, admin_key) {
    e <- new.env(parent = emptyenv())
    e$questions <- new.env(parent = emptyenv())
    # Settings: Inf = unlimited
    e$settings  <- list(
        max_votes_total    = Inf,   # across ALL questions per guest
        max_votes_per_q    = Inf    # per individual question per guest
    )
    e$admin_key <- admin_key
    assign(cid,       e,      envir = store$conferences)
    assign(cid,       0L,     envir = store$user_counts)
    assign(cid,       list(), envir = store$activity_log)
    assign(cid,       list(), envir = store$guests)
    assign(admin_key, cid,    envir = store$admin_keys)
    invisible(e)
}

# ── activity helpers ─────────────────────────────────────────────────────────
add_activity <- function(cid, msg) {
    if (!exists(cid, envir = store$activity_log, inherits = FALSE)) return()
    log <- get(cid, envir = store$activity_log, inherits = FALSE)
    log <- c(list(list(msg = msg, ts = format(Sys.time(), "%H:%M"))), log)
    if (length(log) > 60) log <- log[seq_len(60)]
    assign(cid, log, envir = store$activity_log)
}

# ── guest registry ───────────────────────────────────────────────────────────
register_guest <- function(cid, sid, name) {
    gl <- get(cid, envir = store$guests, inherits = FALSE)
    gl[[sid]] <- list(name = name, kicked = FALSE)
    assign(cid, gl, envir = store$guests)
}
update_guest_name <- function(cid, sid, new_name) {
    gl <- get(cid, envir = store$guests, inherits = FALSE)
    if (!is.null(gl[[sid]])) gl[[sid]]$name <- new_name
    assign(cid, gl, envir = store$guests)
}
remove_guest <- function(cid, sid) {
    gl  <- get(cid, envir = store$guests, inherits = FALSE)
    gl[[sid]] <- NULL
    assign(cid, gl, envir = store$guests)
    cnt <- max(0L, get(cid, envir = store$user_counts, inherits = FALSE) - 1L)
    assign(cid, cnt, envir = store$user_counts)
}
list_guests <- function(cid) {
    if (!exists(cid, envir = store$guests, inherits = FALSE)) return(list())
    get(cid, envir = store$guests, inherits = FALSE)
}

# ── vote-limit helpers ───────────────────────────────────────────────────────
within_total_limit <- function(total_cast, max_total) {
    is.infinite(max_total) || total_cast < max_total
}
within_q_limit <- function(q_cast, max_per_q) {
    is.infinite(max_per_q) || q_cast < max_per_q
}

# ============================================================================
# CSS
# ============================================================================
chorus_css <- '
@import url("https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght@0,9..144,300;0,9..144,600;0,9..144,800;1,9..144,300;1,9..144,600&family=Figtree:wght@300;400;500;600;700&display=swap");

:root {
  --cream:      #f8f5ef;
  --cream-dark: #ede8de;
  --ink:        #1c2b22;
  --ink-mid:    #3d5044;
  --ink-light:  #6b836f;
  --sage:       #7aab85;
  --sage-d:     #5a8f6a;
  --sage-light: #a8c9b0;
  --sage-pale:  #dff0e4;
  --sage-mist:  #eef7f0;
  --gold:       #c8a84b;
  --red-soft:   #c0544a;
  --red-mist:   #fdf0ee;
  --white:      #ffffff;
  --border:     #dce8df;
  --r-lg: 20px; --r-md: 14px; --r-sm: 8px; --r-pill: 999px;
  --sh-sm: 0 2px 12px rgba(28,43,34,0.07);
  --sh-md: 0 8px 32px rgba(28,43,34,0.11);
  --ease: 0.22s cubic-bezier(.4,0,.2,1);
}

*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
html { scroll-behavior: smooth; font-size: 17px; }
body {
  font-family: "Figtree", sans-serif !important;
  background: var(--cream) !important;
  color: var(--ink) !important;
  min-height: 100vh; line-height: 1.6;
}
body::before {
  content: ""; position: fixed; inset: 0; z-index: 0; pointer-events: none;
  background-image:
    radial-gradient(ellipse 80% 60% at 15% 10%, rgba(122,171,133,0.10) 0%, transparent 60%),
    radial-gradient(ellipse 60% 50% at 85% 85%, rgba(200,168,75,0.07) 0%, transparent 60%);
}
.container-fluid { padding: 0 !important; position: relative; z-index: 1; }

/* ── TOP BAR ── */
.topbar {
  position: sticky; top: 0; z-index: 200;
  display: flex; align-items: center; justify-content: space-between;
  padding: 0 2.4rem; height: 66px;
  background: rgba(248,245,239,0.92);
  backdrop-filter: blur(22px) saturate(1.5);
  border-bottom: 1px solid var(--border);
}
.brand { display: flex; align-items: baseline; gap: 0.55rem; }
.brand-name {
  font-family: "Fraunces", serif;
  font-size: 1.5rem; font-weight: 800; letter-spacing: -0.03em; color: var(--ink);
}
.brand-tag { font-size: 0.72rem; font-weight: 600; letter-spacing: 0.08em;
  text-transform: uppercase; color: var(--ink-light); }
.topbar-right { display: flex; align-items: center; gap: 1rem; }
.role-lbl { font-size: 0.75rem; font-weight: 600; color: var(--ink-light);
  text-transform: uppercase; letter-spacing: 0.09em; }

/* ── WRAPPER ── */
.chorus-wrap { max-width: 980px; margin: 0 auto; padding: 2.5rem 2rem 6rem; }

/* ── HERO ── */
.hero { text-align: center; padding: 2.8rem 1rem 2rem; animation: fadeUp .55s ease both; }
.eyebrow { font-size: 0.72rem; font-weight: 700; letter-spacing: 0.18em;
  text-transform: uppercase; color: var(--sage); margin-bottom: 0.55rem; }
.hero-title {
  font-family: "Fraunces", serif;
  font-size: clamp(2.2rem, 5vw, 3.2rem); font-weight: 800; line-height: 1.08;
  color: var(--ink); letter-spacing: -0.03em;
}
.hero-title em { font-style: italic; color: var(--sage); }

/* ── CARD ── */
.pcard {
  background: var(--white); border: 1px solid var(--border);
  border-radius: var(--r-lg); padding: 2rem 2.2rem;
  box-shadow: var(--sh-sm); margin-bottom: 1.4rem;
  animation: fadeUp .5s ease both; transition: box-shadow var(--ease);
}
.pcard:hover { box-shadow: var(--sh-md); }
.pcard-title { font-family: "Fraunces", serif; font-size: 1.25rem; font-weight: 700;
  color: var(--ink); letter-spacing: -0.02em; margin-bottom: 0.25rem; }
.pcard-sub { font-size: 0.92rem; color: var(--ink-light); font-weight: 300;
  line-height: 1.65; margin-bottom: 1.5rem; }

/* ── STEP LIST ── */
.steps { list-style: none; display: flex; flex-direction: column; gap: 0.6rem; margin-bottom: 1.8rem; }
.steps li { display: flex; align-items: flex-start; gap: 0.8rem;
  font-size: 0.95rem; color: var(--ink-mid); line-height: 1.5; }
.step-n { width: 24px; height: 24px; border-radius: 50%; flex-shrink: 0;
  background: var(--sage); color: white; font-size: 0.7rem; font-weight: 700;
  display: flex; align-items: center; justify-content: center; margin-top: 1px; }

/* ── INPUTS ── */
label { font-size: 0.75rem !important; font-weight: 700 !important;
  text-transform: uppercase !important; letter-spacing: 0.1em !important;
  color: var(--ink-light) !important; margin-bottom: 0.4rem !important; display: block !important; }
.form-control {
  border: 1.5px solid var(--border) !important; border-radius: var(--r-sm) !important;
  background: var(--white) !important; font-family: "Figtree", sans-serif !important;
  font-size: 1rem !important; color: var(--ink) !important;
  padding: 0.65rem 0.95rem !important;
  transition: border-color var(--ease), box-shadow var(--ease) !important;
  box-shadow: none !important; width: 100% !important;
}
.form-control:focus {
  border-color: var(--sage) !important;
  box-shadow: 0 0 0 3px rgba(122,171,133,0.20) !important; outline: none !important;
}
.form-group { margin-bottom: 1.2rem !important; }
.token-input .form-control {
  font-family: "Fraunces", serif !important; font-size: 1.5rem !important;
  font-weight: 800 !important; letter-spacing: 0.22em !important;
  text-align: center !important; padding: 0.9rem 1.2rem !important;
  border-radius: var(--r-md) !important;
}

/* ── LIMIT INPUTS — inline row ── */
.limit-row { display: flex; align-items: center; gap: 0.75rem; flex-wrap: wrap; margin-bottom: 1rem; }
.limit-row label { margin-bottom: 0 !important; white-space: nowrap; }
.limit-row .form-control { width: 90px !important; }
.limit-badge {
  display: inline-flex; align-items: center; gap: 0.4rem;
  background: var(--sage-mist); border: 1px solid var(--sage-pale);
  border-radius: var(--r-pill); padding: 0.3rem 0.85rem;
  font-size: 0.82rem; font-weight: 600; color: var(--ink-mid);
}
.limit-badge.unlimited { color: var(--sage-d); }
.limit-badge.limited   { color: var(--gold); background: #fffae8; border-color: #f0d070; }

/* ── BUTTONS ── */
.btn {
  font-family: "Figtree", sans-serif !important; font-size: 0.85rem !important;
  font-weight: 700 !important; letter-spacing: 0.07em !important;
  text-transform: uppercase !important; border-radius: var(--r-pill) !important;
  padding: 0.65rem 1.6rem !important; border: none !important; cursor: pointer !important;
  transition: all var(--ease) !important;
  display: inline-flex !important; align-items: center !important; gap: 0.4rem !important;
}
.btn-sage { background: var(--sage) !important; color: white !important;
  box-shadow: 0 3px 10px rgba(122,171,133,0.35) !important; }
.btn-sage:hover { background: var(--sage-d) !important; transform: translateY(-1px) !important; }
.btn-ink { background: var(--ink) !important; color: var(--cream) !important;
  box-shadow: 0 3px 10px rgba(28,43,34,0.22) !important; }
.btn-ink:hover { background: var(--ink-mid) !important; transform: translateY(-1px) !important; }
.btn-ghost { background: transparent !important; color: var(--ink-light) !important;
  border: 1.5px solid var(--border) !important; }
.btn-ghost:hover { border-color: var(--sage) !important; color: var(--ink) !important;
  background: var(--sage-mist) !important; }
.btn-danger { background: var(--red-mist) !important; color: var(--red-soft) !important;
  border: 1.5px solid rgba(192,84,74,0.2) !important; }
.btn-danger:hover { background: rgba(192,84,74,0.18) !important; }
.btn-sm { padding: 0.38rem 0.85rem !important; font-size: 0.75rem !important; }

/* ── LOCK BANNER ── */
.lock-banner {
  display: flex; align-items: center; gap: 0.6rem;
  background: #fff8e6; border: 1px solid #f0d070; border-radius: var(--r-sm);
  padding: 0.65rem 1rem; font-size: 0.9rem; color: #7a5c00; margin-bottom: 1rem;
}
.lock-icon { font-size: 1rem; }

/* ── TOKEN DISPLAY ── */
.token-box {
  background: var(--ink); border-radius: var(--r-md); padding: 1.6rem 2rem;
  margin-bottom: 1.5rem; position: relative; overflow: hidden;
  animation: fadeUp .4s ease both;
}
.token-box::before {
  content: ""; position: absolute; top: -40px; right: -40px;
  width: 160px; height: 160px; border-radius: 50%;
  background: radial-gradient(circle, rgba(122,171,133,0.22) 0%, transparent 70%);
}
.token-lbl { font-size: 0.7rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.14em; color: var(--sage-light); margin-bottom: 0.5rem; }
.token-val { font-family: "Fraunces", serif; font-size: 2.8rem; font-weight: 800;
  letter-spacing: 0.25em; color: white; line-height: 1; }
.token-hint { font-size: 0.8rem; color: rgba(255,255,255,0.4); margin-top: 0.55rem; font-weight: 300; }
.admin-key-box {
  margin-top: 1rem; padding: 0.8rem 1rem;
  background: rgba(255,255,255,0.07); border-radius: var(--r-sm);
  border: 1px solid rgba(255,255,255,0.12);
}
.admin-key-lbl { font-size: 0.62rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.14em; color: rgba(255,255,255,0.4); margin-bottom: 0.25rem; }
.admin-key-val { font-family: "Fraunces", serif; font-size: 1rem; font-weight: 600;
  letter-spacing: 0.12em; color: rgba(255,255,255,0.65); }
.admin-key-hint { font-size: 0.72rem; color: rgba(255,255,255,0.3); margin-top: 0.2rem; }

/* ── STATUS BAR ── */
.status-bar {
  display: inline-flex; align-items: center; gap: 0.55rem;
  padding: 0.55rem 1.1rem; background: var(--sage-mist);
  border-radius: var(--r-pill); border: 1px solid var(--sage-pale);
  font-size: 0.88rem; font-weight: 500; color: var(--ink-mid); margin-bottom: 1.5rem;
}
.status-dot { width: 9px; height: 9px; border-radius: 50%; background: var(--sage);
  animation: pulse 2s ease-in-out infinite; }
@keyframes pulse { 0%,100%{opacity:1;transform:scale(1)} 50%{opacity:.4;transform:scale(.8)} }

/* ── ADMIN GRIDS ── */
.admin-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 1.3rem; margin-bottom: 1.3rem; }
@media(max-width:720px){ .admin-grid { grid-template-columns: 1fr; } }

/* ── DIVIDER / EYEBROW ── */
.div { height: 1px; background: var(--border); margin: 1.3rem 0; }
.sec-eye { font-size: 0.68rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.16em; color: var(--sage); margin-bottom: 0.4rem; }

/* ── ANSWER CARDS ── */
.answer-list { display: flex; flex-direction: column; gap: 0.75rem; margin-top: 0.5rem; }
.answer-card {
  background: var(--white); border: 1.5px solid var(--border);
  border-radius: var(--r-md); padding: 1rem 1.2rem;
  display: flex; align-items: center; gap: 1rem;
  transition: border-color var(--ease), box-shadow var(--ease), transform var(--ease);
  position: relative; overflow: hidden;
}
.answer-card:hover { border-color: var(--sage-light); box-shadow: var(--sh-sm); transform: translateY(-1px); }
.answer-card.voted { border-color: var(--sage); background: var(--sage-mist); }
.answer-card.admin-card { border-color: var(--gold); background: #fffdf5; }
.answer-bar-bg {
  position: absolute; left: 0; top: 0; bottom: 0; border-radius: var(--r-md);
  background: linear-gradient(90deg, rgba(122,171,133,0.13), rgba(122,171,133,0.04));
  transition: width 0.55s cubic-bezier(.4,0,.2,1); z-index: 0;
}
.answer-content { flex: 1; position: relative; z-index: 1; min-width: 0; }
.answer-text { font-size: 1rem; font-weight: 500; color: var(--ink); line-height: 1.4; }
.answer-meta { font-size: 0.78rem; color: var(--ink-light); margin-top: 0.15rem; }
.admin-tag {
  display: inline-block; font-size: 0.62rem; font-weight: 700;
  text-transform: uppercase; letter-spacing: 0.08em;
  background: #fdf3d0; color: #b8860b; border-radius: 99px;
  padding: 0.12rem 0.5rem; margin-left: 0.4rem; vertical-align: middle;
}
.answer-vote-controls { display: flex; align-items: center; gap: 0.5rem; position: relative; z-index: 1; flex-shrink: 0; }
.vote-count { font-family: "Fraunces", serif; font-size: 1.3rem; font-weight: 700;
  color: var(--ink); min-width: 2.2rem; text-align: center; line-height: 1; }
.vbtn {
  width: 38px; height: 38px; border-radius: 50%; border: none !important;
  cursor: pointer !important; font-size: 1.25rem; font-weight: 700; line-height: 1;
  display: flex; align-items: center; justify-content: center;
  transition: all var(--ease) !important; padding: 0 !important;
}
.vbtn-plus  { background: var(--sage) !important; color: white !important;
  box-shadow: 0 2px 8px rgba(122,171,133,0.35) !important; }
.vbtn-plus:hover  { background: var(--sage-d) !important; transform: scale(1.12) !important; }
.vbtn-minus { background: var(--cream-dark) !important; color: var(--red-soft) !important; }
.vbtn-minus:hover { background: #f5ddd9 !important; transform: scale(1.12) !important; }
.vbtn-del   { background: var(--red-mist) !important; color: var(--red-soft) !important;
  font-size: 0.85rem !important; width: 30px !important; height: 30px !important; }
.vbtn-del:hover { background: rgba(192,84,74,0.2) !important; }

/* ── VOTE MSG ── */
.vote-msg { font-size: 0.88rem; font-weight: 500; color: var(--red-soft);
  background: var(--red-mist); border-radius: var(--r-sm);
  padding: 0.55rem 1rem; margin-top: 0.8rem;
  border: 1px solid rgba(192,84,74,0.18); display: inline-block; }

/* ── VOTE BUDGET DISPLAY ── */
.vote-budget {
  display: flex; gap: 0.75rem; flex-wrap: wrap; margin-bottom: 1rem;
}
.budget-pill {
  display: inline-flex; align-items: center; gap: 0.35rem;
  padding: 0.35rem 0.9rem; border-radius: var(--r-pill);
  font-size: 0.82rem; font-weight: 600;
  background: var(--sage-mist); border: 1px solid var(--sage-pale); color: var(--ink-mid);
}
.budget-pill.warn { background: #fff8e6; border-color: #f0d070; color: #7a5c00; }
.budget-dot { width: 7px; height: 7px; border-radius: 50%; background: var(--sage); }
.budget-dot.warn { background: var(--gold); }

/* ── KICK / RENAME SCREEN ── */
.kicked-screen { text-align: center; padding: 4rem 2rem; animation: fadeUp .4s ease both; }
.kicked-icon  { font-size: 3rem; margin-bottom: 1rem; }
.kicked-title { font-family: "Fraunces", serif; font-size: 1.8rem; font-weight: 700;
  color: var(--ink); margin-bottom: 0.5rem; }
.kicked-sub   { font-size: 1rem; color: var(--ink-light); font-weight: 300; }

/* ── NAME BADGE ── */
.name-badge { display: inline-flex; align-items: center; gap: 0.5rem;
  background: var(--sage-mist); border: 1px solid var(--sage-pale);
  border-radius: var(--r-pill); padding: 0.4rem 1rem;
  font-size: 0.88rem; font-weight: 500; color: var(--ink-mid); margin-bottom: 1rem; }
.name-badge-dot { font-size: 0.7rem; color: var(--sage); }

/* ── ACTIVITY FEED ── */
.activity-feed { background: var(--white); border: 1px solid var(--border);
  border-radius: var(--r-md); padding: 1rem 1.2rem;
  max-height: 240px; overflow-y: auto;
  display: flex; flex-direction: column; gap: 0.45rem; }
.activity-feed::-webkit-scrollbar { width: 4px; }
.activity-feed::-webkit-scrollbar-thumb { background: var(--border); border-radius: 4px; }
.activity-item { display: flex; gap: 0.6rem; align-items: flex-start;
  font-size: 0.85rem; line-height: 1.4;
  padding-bottom: 0.45rem; border-bottom: 1px solid var(--border); }
.activity-item:last-child { border-bottom: none; padding-bottom: 0; }
.activity-ts  { font-size: 0.72rem; color: var(--ink-light); flex-shrink: 0; margin-top: 1px; }
.activity-msg { color: var(--ink-mid); }
.activity-msg strong { color: var(--ink); font-weight: 600; }
.activity-empty { font-size: 0.88rem; color: var(--ink-light); font-style: italic; padding: 0.3rem 0; }

/* ── GUEST TABLE ── */
.guest-table { width: 100%; border-collapse: collapse; }
.guest-table th { font-size: 0.72rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.1em; color: var(--ink-light); background: var(--sage-mist);
  border-bottom: 1px solid var(--border); padding: 0.65rem 1rem; text-align: left; }
.guest-table td { font-size: 0.92rem; padding: 0.65rem 1rem;
  border-bottom: 1px solid var(--border); vertical-align: middle; }
.guest-table tr:last-child td { border-bottom: none; }

/* ── TOP ANSWERS TABLE ── */
.topans-table { width: 100%; border-collapse: collapse; }
.topans-table thead th { font-size: 0.72rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.1em; color: var(--ink-light); background: var(--sage-mist);
  border-bottom: 1px solid var(--border); padding: 0.75rem 1rem; text-align: left; }
.topans-table tbody tr:hover { background: var(--sage-mist); }
.topans-table tbody td { font-size: 0.92rem; color: var(--ink);
  padding: 0.75rem 1rem; border-bottom: 1px solid var(--border); }
.topans-table tbody tr:last-child td { border-bottom: none; }
.rank-badge { display: inline-flex; align-items: center; justify-content: center;
  width: 26px; height: 26px; border-radius: 50%;
  font-family: "Fraunces", serif; font-size: 0.85rem; font-weight: 700; }
.rank-1 { background: #fdf3d0; color: #b8860b; }
.rank-2 { background: #f0f0f0; color: #666; }
.rank-3 { background: #fde8d8; color: #c06020; }
.rank-n { background: var(--sage-mist); color: var(--ink-light); font-size: 0.75rem; }
.mini-bar-wrap { width: 90px; height: 8px; background: var(--cream-dark); border-radius: 4px; overflow: hidden; }
.mini-bar { height: 100%; background: linear-gradient(90deg, var(--sage), var(--sage-light));
  border-radius: 4px; transition: width 0.6s ease; }

/* ── DT overrides ── */
table.dataTable { font-family: "Figtree", sans-serif !important; border-collapse: collapse !important; }
table.dataTable thead th { font-size: 0.75rem !important; font-weight: 700 !important;
  text-transform: uppercase !important; letter-spacing: 0.1em !important;
  color: var(--ink-light) !important; background: var(--sage-mist) !important;
  border-bottom: 1px solid var(--border) !important; padding: 0.75rem 1rem !important; }
table.dataTable tbody td { font-size: 0.95rem !important; padding: 0.75rem 1rem !important;
  border-bottom: 1px solid var(--border) !important; }
table.dataTable tbody tr:hover { background: var(--sage-mist) !important; }
.dataTables_wrapper { padding: 0 !important; }
.dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate {
  font-size: 0.82rem !important; font-family: "Figtree", sans-serif !important;
  color: var(--ink-light) !important; padding: 0.75rem 0 !important; }

/* ── SELECTIZE ── */
.selectize-input { border: 1.5px solid var(--border) !important; border-radius: var(--r-sm) !important;
  font-family: "Figtree", sans-serif !important; font-size: 1rem !important;
  color: var(--ink) !important; padding: 0.65rem 0.95rem !important;
  box-shadow: none !important; background: var(--white) !important; }
.selectize-input:focus-within { border-color: var(--sage) !important;
  box-shadow: 0 0 0 3px rgba(122,171,133,0.2) !important; }
.selectize-dropdown { font-family: "Figtree", sans-serif !important; font-size: 0.95rem !important;
  border-radius: var(--r-sm) !important; border: 1.5px solid var(--border) !important;
  box-shadow: var(--sh-md) !important; }
.selectize-dropdown .option:hover, .selectize-dropdown .option.active {
  background: var(--sage-mist) !important; color: var(--ink) !important; }
.topbar-sel .selectize-input { border-radius: 999px !important;
  padding: 0.38rem 1.1rem !important; font-size: 0.88rem !important; font-weight: 600 !important; }

/* ── MISC ── */
.well { background: transparent !important; border: none !important;
  box-shadow: none !important; padding: 0 !important; }
.checkbox label { font-size: 0.95rem !important; font-weight: 400 !important;
  text-transform: none !important; letter-spacing: 0 !important; color: var(--ink-mid) !important; }
input[type="checkbox"] { accent-color: var(--sage); transform: scale(1.2); }
.shiny-notification { font-family: "Figtree", sans-serif !important; font-size: 0.92rem !important;
  border-radius: var(--r-md) !important; border-left: 4px solid var(--sage) !important; }

/* ── FOOTER ── */
.chorus-foot { text-align: center; padding: 2.5rem 2rem; border-top: 1px solid var(--border);
  margin-top: 2rem; font-size: 0.85rem; color: var(--ink-light); font-weight: 300; }
.chorus-foot a { color: var(--sage); text-decoration: none; font-weight: 600; }
.chorus-foot a:hover { text-decoration: underline; }
.foot-note { margin-top: 0.6rem; font-size: 0.75rem; background: var(--sage-mist);
  border-radius: 99px; padding: 0.38rem 1.1rem; display: inline-block; }

@keyframes fadeUp { from{opacity:0;transform:translateY(16px)} to{opacity:1;transform:translateY(0)} }
'

# ============================================================================
# HELPERS
# ============================================================================

# Render a numeric-or-infinite limit input row
limit_input_ui <- function(input_id, label_text, current_val, checkbox_id) {
    is_inf <- is.infinite(current_val)
    tagList(
        tags$div(class = "sec-eye", label_text),
        tags$div(class = "limit-row",
                 checkboxInput(checkbox_id, "Unlimited", value = is_inf),
                 if (!is_inf)
                     numericInput(input_id, label = NULL, value = current_val, min = 1, width = "90px")
                 else
                     tags$span(class = "limit-badge unlimited", "∞  No limit")
        )
    )
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
    tags$head(tags$style(HTML(chorus_css))),

    # TOP BAR
    tags$div(class = "topbar",
             tags$div(class = "brand",
                      tags$span(class = "brand-name", "Chorus"),
                      tags$span(class = "brand-tag", "Live Polling")
             ),
             tags$div(class = "topbar-right",
                      tags$span(class = "role-lbl", "I am a"),
                      tags$div(class = "topbar-sel",
                               selectInput("user_role", label = NULL,
                                           choices = c("Guest", "Admin"), selected = "Guest", width = "150px")
                      )
             )
    ),

    tags$div(class = "chorus-wrap",

             tags$div(class = "hero",
                      tags$div(class = "eyebrow", "Participative · Live · Democratic"),
                      tags$h1(class = "hero-title", "Every voice", tags$br(), tags$em("counts."))
             ),

             # ── GUEST PANEL ──────────────────────────────────────────────────────────
             conditionalPanel("input.user_role == 'Guest'",

                              conditionalPanel("!output.validGuestConf",
                                               tags$div(class = "pcard",
                                                        tags$div(class = "pcard-title", "Join a session"),
                                                        tags$div(class = "pcard-sub", "Enter the token your organizer shared."),
                                                        tags$ul(class = "steps",
                                                                tags$li(tags$span(class="step-n","1"), "Get the 4-character token from your organizer"),
                                                                tags$li(tags$span(class="step-n","2"), "Optionally enter your name so others can see you"),
                                                                tags$li(tags$span(class="step-n","3"), "Vote, suggest answers, and watch results live")
                                                        ),
                                                        tags$div(class = "token-input",
                                                                 textInput("guest_token_input", "Access Token", placeholder = "")
                                                        ),
                                                        textInput("guest_name_input", "Your name (optional)", placeholder = ""),
                                                        actionButton("guest_enter", "Join Room", class = "btn btn-sage")
                                               )
                              ),

                              conditionalPanel("output.validGuestConf",
                                               uiOutput("kicked_ui"),
                                               conditionalPanel("!output.isKicked",
                                                                uiOutput("guest_name_badge_ui"),
                                                                uiOutput("user_count_ui_guest"),
                                                                tags$div(class = "pcard",
                                                                         tags$div(class = "sec-eye", "Active Question"),
                                                                         uiOutput("question_ui_guest"),
                                                                         tags$div(class = "div"),
                                                                         uiOutput("guest_vote_budget_ui"),
                                                                         uiOutput("guest_answer_section")
                                                                ),
                                                                tags$div(class = "pcard",
                                                                         tags$div(class = "sec-eye", "Live Activity"),
                                                                         tags$div(class = "pcard-title", style = "margin-bottom:.75rem;", "What's happening"),
                                                                         uiOutput("activity_feed_guest")
                                                                )
                                               )
                              )
             ),

             # ── ADMIN PANEL ──────────────────────────────────────────────────────────
             conditionalPanel("input.user_role == 'Admin'",

                              conditionalPanel("!output.isAdmin",
                                               tags$div(class = "pcard",
                                                        tags$div(class = "pcard-title", "Create or resume a room"),
                                                        tags$div(class = "pcard-sub", "Start fresh or re-enter your admin key to resume after a reload."),
                                                        tags$ul(class = "steps",
                                                                tags$li(tags$span(class="step-n","1"), "Enter a short conference ID to create a new room"),
                                                                tags$li(tags$span(class="step-n","2"), "— or paste your Admin Key to resume an existing room"),
                                                                tags$li(tags$span(class="step-n","3"), "Share the room token with attendees and start polling")
                                                        ),
                                                        textInput("new_conf_id", "Conference ID (new room)", placeholder = "e.g. SciConf2025"),
                                                        actionButton("create_conf", "Create Room", class = "btn btn-sage"),
                                                        tags$div(class = "div"),
                                                        textInput("resume_key_input", "Admin Key (resume existing room)", placeholder = "Paste your admin key"),
                                                        actionButton("resume_conf", "Resume Room", class = "btn btn-ghost")
                                               )
                              ),

                              conditionalPanel("output.isAdmin",
                                               uiOutput("admin_token_display"),
                                               uiOutput("user_count_ui"),

                                               tags$div(class = "admin-grid",

                                                        # Questions
                                                        tags$div(class = "pcard",
                                                                 tags$div(class = "sec-eye", "Questions"),
                                                                 tags$div(class = "pcard-title", "Manage questions"),
                                                                 tags$div(class = "div"),
                                                                 textInput("new_question_text", "New question", placeholder = "Ask something…"),
                                                                 actionButton("add_question", "Add", class = "btn btn-sage"),
                                                                 tags$div(class = "div"),
                                                                 selectInput("delete_question_id", "Delete question", choices = NULL),
                                                                 actionButton("delete_question", "Delete", class = "btn btn-ghost"),
                                                                 tags$br(), tags$br(),
                                                                 selectInput("restart_question_id", "Reset responses for", choices = NULL),
                                                                 actionButton("restart_question", "Reset", class = "btn btn-ghost")
                                                        ),

                                                        # Settings
                                                        tags$div(class = "pcard",
                                                                 tags$div(class = "sec-eye", "Settings"),
                                                                 tags$div(class = "pcard-title", "Voting rules"),
                                                                 tags$div(class = "div"),
                                                                 # Total votes across all questions
                                                                 uiOutput("setting_total_ui"),
                                                                 tags$br(),
                                                                 # Votes per question
                                                                 uiOutput("setting_per_q_ui"),
                                                                 tags$div(class = "div"),
                                                                 actionButton("apply_settings", "Save Settings", class = "btn btn-ink")
                                                        )
                                               ),

                                               # Live results + admin answer controls
                                               tags$div(class = "pcard",
                                                        tags$div(class = "sec-eye", "Live Results"),
                                                        tags$div(class = "pcard-title", style = "margin-bottom:.75rem;", "Question breakdown"),
                                                        uiOutput("question_ui_admin"),
                                                        tags$div(class = "div"),

                                                        # Admin answer suggest + lock controls
                                                        tags$div(class = "admin-grid", style = "margin-bottom:1rem;",
                                                                 tags$div(
                                                                     tags$div(class = "sec-eye", "Suggest an answer"),
                                                                     tags$div(style = "display:flex;gap:.7rem;align-items:flex-end;",
                                                                              tags$div(style = "flex:1;",
                                                                                       textInput("admin_answer_text", label = NULL, placeholder = "Add an answer for this question…")
                                                                              ),
                                                                              actionButton("admin_submit_answer", "Add", class = "btn btn-ink",
                                                                                           style = "margin-bottom:1rem;flex-shrink:0;")
                                                                     )
                                                                 ),
                                                                 tags$div(
                                                                     tags$div(class = "sec-eye", "Guest suggestions"),
                                                                     uiOutput("lock_toggle_ui"),
                                                                     tags$p(style="font-size:.82rem;color:var(--ink-light);margin-top:.5rem;",
                                                                            "When locked, guests can still vote but cannot add new answers.")
                                                                 )
                                                        ),
                                                        tags$div(class = "div"),
                                                        DTOutput("admin_table"),
                                                        tags$p(style="font-size:.8rem;color:var(--ink-light);margin-top:.6rem;",
                                                               "Click the 🗑 button next to any answer to delete it.")
                                               ),

                                               # Top X
                                               tags$div(class = "pcard",
                                                        tags$div(class = "sec-eye", "Cross-Question Summary"),
                                                        tags$div(class = "pcard-title", "Top answers across all questions"),
                                                        tags$div(style = "display:flex;align-items:center;gap:1rem;margin:.75rem 0 1.2rem;",
                                                                 tags$span(style="font-size:.9rem;color:var(--ink-light);", "Show top"),
                                                                 numericInput("top_n", label = NULL, value = 5, min = 1, max = 50, width = "90px"),
                                                                 tags$span(style="font-size:.9rem;color:var(--ink-light);", "answers")
                                                        ),
                                                        uiOutput("top_answers_ui")
                                               ),

                                               # Guest management
                                               tags$div(class = "pcard",
                                                        tags$div(class = "sec-eye", "Attendees"),
                                                        tags$div(class = "pcard-title", style = "margin-bottom:.75rem;", "Manage guests"),
                                                        uiOutput("guest_mgmt_ui")
                                               ),

                                               # Export
                                               tags$div(class = "pcard",
                                                        tags$div(class = "sec-eye", "Export"),
                                                        tags$div(class = "pcard-title", "Download session data"),
                                                        tags$p(style="font-size:.92rem;color:var(--ink-light);margin-bottom:1.2rem;",
                                                               "Export all questions and responses as a CSV file."),
                                                        downloadButton("export_csv", "Download CSV", class = "btn btn-ink")
                                               ),

                                               # Activity
                                               tags$div(class = "pcard",
                                                        tags$div(class = "sec-eye", "Live Activity"),
                                                        tags$div(class = "pcard-title", style = "margin-bottom:.75rem;", "Room activity"),
                                                        uiOutput("activity_feed_admin")
                                               )
                              )
             )
    ),

    tags$div(class = "chorus-foot",
             tags$p("Chorus by ",
                    tags$a("Dr. Cristian Román-Palacios · Data Diversity Lab, U of A",
                           href = "https://datadiversitylab.github.io/", target = "_blank"),
                    " · ", tags$a("GitHub", href = "https://github.com/datadiversitylab/polapp", target = "_blank")
             ),
             tags$p("Concept by ",
                    tags$a("Dr. Heidi Steiner", href = "https://heidiesteiner.netlify.app/", target = "_blank")),
             tags$div(class = "foot-note", "✦ No data stored permanently unless you export before closing")
    )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

    sid <- paste0("s_", as.integer(Sys.time()), "_", sample(1e6, 1))

    is_admin      <- reactiveVal(FALSE)
    admin_cid     <- reactiveVal(NULL)
    guest_cid     <- reactiveVal(NULL)
    guest_qsel    <- reactiveVal(NULL)
    guest_name    <- reactiveVal("")
    user_votes    <- reactiveVal(list())   # qid -> named int (aid_char -> n_votes)
    vote_msg      <- reactiveVal("")
    is_kicked     <- reactiveVal(FALSE)
    rename_target <- reactiveVal(NULL)

    output$isAdmin        <- reactive({ is_admin() })
    output$validGuestConf <- reactive({ !is.null(guest_cid()) })
    output$isKicked       <- reactive({ is_kicked() })
    outputOptions(output, "isAdmin",        suspendWhenHidden = FALSE)
    outputOptions(output, "validGuestConf", suspendWhenHidden = FALSE)
    outputOptions(output, "isKicked",       suspendWhenHidden = FALSE)

    trig_g <- reactive({ cid <- guest_cid(); req(cid); get_trigger(cid)() })
    trig_a <- reactive({ cid <- admin_cid(); req(cid); get_trigger(cid)() })

    # ── Kick polling ─────────────────────────────────────────────────────────
    observe({
        invalidateLater(2000, session)
        if (isTRUE(get_kick_trig(sid)())) is_kicked(TRUE)
    })

    output$kicked_ui <- renderUI({
        if (!is_kicked()) return(NULL)
        tags$div(class = "kicked-screen",
                 tags$div(class = "kicked-icon", "🎵"),
                 tags$div(class = "kicked-title", "You've been removed"),
                 tags$div(class = "kicked-sub",
                          "The organizer has ended your session. Thank you for participating in Chorus.")
        )
    })

    # ── ADMIN: create ─────────────────────────────────────────────────────────
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

    # ── ADMIN: resume ─────────────────────────────────────────────────────────
    observeEvent(input$resume_conf, {
        key <- trimws(input$resume_key_input)
        if (!exists(key, envir = store$admin_keys, inherits = FALSE)) {
            showNotification("Admin key not found. Room may have expired.", type = "error"); return()
        }
        cid <- get(key, envir = store$admin_keys, inherits = FALSE)
        is_admin(TRUE); admin_cid(cid)
        bump(cid)
        showNotification(paste("Resumed room:", cid))
    })

    # ── Token display ─────────────────────────────────────────────────────────
    output$admin_token_display <- renderUI({
        req(admin_cid())
        cid   <- admin_cid(); e <- get_conf(cid); req(e)
        token <- names(Filter(function(v) v == cid, as.list(store$tokens)))[1]
        tags$div(class = "token-box",
                 tags$div(class = "token-lbl", "Share this token with attendees"),
                 tags$div(class = "token-val", token %||% "—"),
                 tags$div(class = "token-hint", paste0("Room: ", cid, " · Anyone with this code can join")),
                 tags$div(class = "admin-key-box",
                          tags$div(class = "admin-key-lbl", "Admin Key — save this to resume after a reload"),
                          tags$div(class = "admin-key-val", e$admin_key),
                          tags$div(class = "admin-key-hint", "Keep it private. Paste it in the resume box to reclaim this room.")
                 )
        )
    })

    # ── GUEST: enter ──────────────────────────────────────────────────────────
    observeEvent(input$guest_enter, {
        token <- trimws(input$guest_token_input)
        nm    <- trimws(input$guest_name_input)
        if (!exists(token, envir = store$tokens, inherits = FALSE)) {
            showNotification("Invalid token — check with your organizer.", type = "error"); return()
        }
        cid  <- get(token, envir = store$tokens, inherits = FALSE)
        disp <- if (nchar(nm) > 0) nm else paste0("Guest-", substr(sid, nchar(sid)-3, nchar(sid)))
        guest_cid(cid); guest_name(disp)
        cur <- get(cid, envir = store$user_counts, inherits = FALSE)
        assign(cid, cur + 1L, envir = store$user_counts)
        register_guest(cid, sid, disp)
        add_activity(cid, paste0("<strong>", disp, "</strong> joined the room"))
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

    # ── User counts ───────────────────────────────────────────────────────────
    output$user_count_ui <- renderUI({
        trig_a(); cid <- admin_cid(); if (is.null(cid)) return(NULL)
        n <- get(cid, envir = store$user_counts, inherits = FALSE) %||% 0L
        tags$div(class = "status-bar", tags$div(class = "status-dot"),
                 paste0(n, " attendee", if (n != 1) "s" else "", " online in ", cid))
    })
    output$user_count_ui_guest <- renderUI({
        trig_g(); cid <- guest_cid(); if (is.null(cid)) return(NULL)
        n <- get(cid, envir = store$user_counts, inherits = FALSE) %||% 0L
        tags$div(class = "status-bar", tags$div(class = "status-dot"),
                 paste0(n, " attendee", if (n != 1) "s" else "", " in this room"))
    })
    output$guest_name_badge_ui <- renderUI({
        nm <- guest_name(); if (nchar(nm) == 0) return(NULL)
        tags$div(class = "name-badge",
                 tags$span(class = "name-badge-dot", "●"), paste0("Participating as: ", nm))
    })

    # ── Settings UI ───────────────────────────────────────────────────────────
    output$setting_total_ui <- renderUI({
        trig_a(); cid <- admin_cid(); req(cid)
        e <- get_conf(cid); req(e)
        cur <- e$settings$max_votes_total
        is_inf <- is.infinite(cur)
        tagList(
            tags$div(class = "sec-eye", "Max votes per attendee (all questions)"),
            tags$div(class = "limit-row",
                     checkboxInput("total_unlimited", "Unlimited", value = is_inf),
                     if (!is_inf)
                         numericInput("max_votes_total", label = NULL, value = cur, min = 1, width = "90px")
                     else
                         tags$span(class = "limit-badge unlimited", "∞  No limit set")
            )
        )
    })

    output$setting_per_q_ui <- renderUI({
        trig_a(); cid <- admin_cid(); req(cid)
        e <- get_conf(cid); req(e)
        cur <- e$settings$max_votes_per_q
        is_inf <- is.infinite(cur)
        tagList(
            tags$div(class = "sec-eye", "Max votes per attendee per question"),
            tags$div(class = "limit-row",
                     checkboxInput("per_q_unlimited", "Unlimited", value = is_inf),
                     if (!is_inf)
                         numericInput("max_votes_per_q", label = NULL, value = cur, min = 1, width = "90px")
                     else
                         tags$span(class = "limit-badge unlimited", "∞  No limit set")
            )
        )
    })

    observeEvent(input$apply_settings, {
        req(admin_cid())
        cid <- admin_cid(); e <- get_conf(cid)
        new_total <- if (isTRUE(input$total_unlimited)) Inf
        else max(1L, as.integer(input$max_votes_total %||% 1L))
        new_per_q <- if (isTRUE(input$per_q_unlimited))  Inf
        else max(1L, as.integer(input$max_votes_per_q  %||% 1L))
        e$settings <- list(max_votes_total = new_total, max_votes_per_q = new_per_q)
        bump(cid); showNotification("Settings saved.")
    })

    # ── Guest vote budget display ─────────────────────────────────────────────
    output$guest_vote_budget_ui <- renderUI({
        trig_g()
        cid <- guest_cid(); qid <- guest_qsel(); req(cid, qid)
        e   <- get_conf(cid); req(e)
        s   <- e$settings
        va  <- user_votes()
        total_cast <- sum(unlist(lapply(va, function(v) sum(v, na.rm = TRUE))), na.rm = TRUE)
        q_cast     <- sum(va[[qid]], na.rm = TRUE)

        total_ok <- is.infinite(s$max_votes_total) || total_cast < s$max_votes_total
        per_q_ok <- is.infinite(s$max_votes_per_q) || q_cast  < s$max_votes_per_q

        total_label <- if (is.infinite(s$max_votes_total))
            paste0("Total: ", total_cast, " votes cast (unlimited)")
        else
            paste0("Total: ", total_cast, " / ", s$max_votes_total)

        per_q_label <- if (is.infinite(s$max_votes_per_q))
            paste0("This question: ", q_cast, " votes cast (unlimited)")
        else
            paste0("This question: ", q_cast, " / ", s$max_votes_per_q)

        tags$div(class = "vote-budget",
                 tags$div(class = paste("budget-pill", if (!total_ok) "warn" else ""),
                          tags$div(class = paste("budget-dot", if (!total_ok) "warn" else "")),
                          total_label
                 ),
                 tags$div(class = paste("budget-pill", if (!per_q_ok) "warn" else ""),
                          tags$div(class = paste("budget-dot", if (!per_q_ok) "warn" else "")),
                          per_q_label
                 )
        )
    })

    # ── Questions ─────────────────────────────────────────────────────────────
    qs_a <- reactive({ trig_a(); cid <- admin_cid(); req(cid); e <- get_conf(cid); req(e); as.list(e$questions) })
    qs_g <- reactive({ trig_g(); cid <- guest_cid(); req(cid); e <- get_conf(cid); req(e); as.list(e$questions) })

    observe({
        qs <- qs_a()
        if (length(qs) == 0) {
            updateSelectInput(session, "delete_question_id",  choices = character(0))
            updateSelectInput(session, "restart_question_id", choices = character(0))
            return()
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
            text      = txt,
            locked    = FALSE,
            responses = data.frame(ID=integer(), Answer=character(), Votes=integer(),
                                   Submitter=character(), IsAdmin=logical(),
                                   stringsAsFactors=FALSE)
        ), envir = e$questions)
        updateTextInput(session, "new_question_text", value = "")
        add_activity(cid, paste0("Question added: <strong>", txt, "</strong>"))
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
                                  Submitter=character(), IsAdmin=logical(), stringsAsFactors=FALSE)
        assign(qid, q, envir = get_conf(cid)$questions)
        bump(cid); showNotification("Responses reset.")
    })

    # ── Admin: question selector ──────────────────────────────────────────────
    output$question_ui_admin <- renderUI({
        qs <- qs_a()
        if (length(qs) == 0) return(tags$p(style="color:var(--ink-light);font-size:.95rem;","No questions yet."))
        selectInput("question_id_admin", "Select question to view:",
                    choices = setNames(names(qs), sapply(qs, `[[`, "text")))
    })

    # ── Lock toggle ───────────────────────────────────────────────────────────
    output$lock_toggle_ui <- renderUI({
        trig_a(); cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
        e <- get_conf(cid); req(e)
        if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
        q <- get(qid, envir = e$questions, inherits = FALSE)
        locked <- isTRUE(q$locked)
        if (locked)
            tags$div(
                tags$div(class = "lock-banner",
                         tags$span(class = "lock-icon", "🔒"),
                         "Suggestions are locked for this question."
                ),
                actionButton("toggle_lock", "Unlock suggestions", class = "btn btn-ghost btn-sm")
            )
        else
            actionButton("toggle_lock", "🔒 Lock suggestions", class = "btn btn-ghost btn-sm")
    })

    observeEvent(input$toggle_lock, {
        cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
        e   <- get_conf(cid)
        q   <- get(qid, envir = e$questions, inherits = FALSE)
        q$locked <- !isTRUE(q$locked)
        assign(qid, q, envir = e$questions)
        status <- if (q$locked) "locked" else "unlocked"
        add_activity(cid, paste0("Suggestions <strong>", status, "</strong> for: <em>", q$text, "</em>"))
        bump(cid); showNotification(paste("Suggestions", status, "for this question."))
    })

    # ── Admin: suggest answer ─────────────────────────────────────────────────
    observeEvent(input$admin_submit_answer, {
        cid <- admin_cid(); qid <- input$question_id_admin
        ans <- trimws(input$admin_answer_text)
        req(cid, qid, nchar(ans) > 0)
        e   <- get_conf(cid)
        q   <- get(qid, envir = e$questions, inherits = FALSE)
        new_id <- if (nrow(q$responses) == 0) 1L else max(q$responses$ID) + 1L
        q$responses <- rbind(q$responses,
                             data.frame(ID=new_id, Answer=ans, Votes=0L,
                                        Submitter="Organizer", IsAdmin=TRUE, stringsAsFactors=FALSE))
        assign(qid, q, envir = e$questions)
        updateTextInput(session, "admin_answer_text", value = "")
        add_activity(cid, paste0("Organizer suggested: <em>", ans, "</em>"))
        bump(cid); showNotification("Answer added.")
    })

    # ── Admin: delete answer ──────────────────────────────────────────────────
    observeEvent(input$delete_answer_action, {
        cid <- admin_cid(); req(cid)
        da  <- input$delete_answer_action
        qid <- da$qid; aid_char <- as.character(da$aid); req(qid, aid_char)
        e   <- get_conf(cid)
        q   <- get(qid, envir = e$questions, inherits = FALSE)
        keep <- as.character(q$responses$ID) != aid_char
        deleted_ans <- q$responses$Answer[!keep]
        q$responses <- q$responses[keep, , drop = FALSE]
        assign(qid, q, envir = e$questions)
        add_activity(cid, paste0("Answer deleted: <em>", deleted_ans[1], "</em>"))
        bump(cid); showNotification("Answer deleted.", type = "warning")
    })

    # ── Admin results table (with delete buttons) ─────────────────────────────
    output$admin_table <- renderDT({
        trig_a(); cid <- admin_cid(); qid <- input$question_id_admin; req(cid, qid)
        e <- get_conf(cid); req(e)
        if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
        r <- get(qid, envir = e$questions, inherits = FALSE)$responses
        if (nrow(r) == 0)
            return(datatable(data.frame(Message="No responses yet"), rownames=FALSE, options=list(dom="t")))

        r_ord <- r[order(-r$Votes), ]
        # Build delete-button column using JS onclick
        del_btns <- sapply(seq_len(nrow(r_ord)), function(i) {
            aid <- r_ord$ID[i]
            sprintf('<button class="vbtn vbtn-del" title="Delete this answer"
        onclick="Shiny.setInputValue(\'delete_answer_action\',{aid:%d,qid:\'%s\'},{priority:\'event\'})">🗑</button>',
                    aid, qid)
        })
        out <- data.frame(
            Answer    = r_ord$Answer,
            Votes     = r_ord$Votes,
            By        = ifelse(isTRUE(r_ord$IsAdmin), "★ Organizer", r_ord$Submitter),
            Delete    = del_btns,
            stringsAsFactors = FALSE
        )
        datatable(out, rownames = FALSE, escape = FALSE,
                  options = list(dom = "tp", pageLength = 15,
                                 columnDefs = list(list(orderable = FALSE, targets = 3))))
    })

    # ── Top X ─────────────────────────────────────────────────────────────────
    output$top_answers_ui <- renderUI({
        trig_a(); cid <- admin_cid(); req(cid)
        e <- get_conf(cid); req(e)
        qs <- as.list(e$questions)
        if (length(qs) == 0) return(tags$p(style="color:var(--ink-light);font-size:.95rem;","No questions yet."))
        n_top <- max(1L, as.integer(input$top_n %||% 5L))
        all_rows <- do.call(rbind, lapply(names(qs), function(qid) {
            q <- qs[[qid]]; r <- q$responses
            if (nrow(r) == 0) return(NULL)
            r$Question <- q$text; r
        }))
        if (is.null(all_rows) || nrow(all_rows) == 0)
            return(tags$p(style="color:var(--ink-light);font-size:.95rem;","No answers yet."))
        top   <- head(all_rows[order(-all_rows$Votes), ], n_top)
        max_v <- max(top$Votes, 1)
        rows  <- lapply(seq_len(nrow(top)), function(i) {
            rc  <- if(i==1)"rank-1" else if(i==2)"rank-2" else if(i==3)"rank-3" else "rank-n"
            pct <- round(top$Votes[i] / max_v * 100)
            tags$tr(
                tags$td(tags$span(class=paste("rank-badge",rc), i)),
                tags$td(style="font-weight:500;", top$Answer[i]),
                tags$td(style="color:var(--ink-light);font-size:.85rem;max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;", top$Question[i]),
                tags$td(style="font-family:'Fraunces',serif;font-weight:700;text-align:right;", top$Votes[i]),
                tags$td(tags$div(class="mini-bar-wrap", tags$div(class="mini-bar",style=paste0("width:",pct,"%;"))))
            )
        })
        tags$table(class="topans-table",
                   tags$thead(tags$tr(tags$th("#"),tags$th("Answer"),tags$th("Question"),
                                      tags$th(style="text-align:right;","Votes"),tags$th("Share"))),
                   tags$tbody(rows)
        )
    })

    # ── Guest management ──────────────────────────────────────────────────────
    output$guest_mgmt_ui <- renderUI({
        trig_a(); cid <- admin_cid(); req(cid)
        gl <- list_guests(cid)
        if (length(gl) == 0)
            return(tags$p(style="color:var(--ink-light);font-size:.92rem;","No guests connected yet."))
        rows <- lapply(names(gl), function(gsid) {
            g <- gl[[gsid]]
            tags$tr(
                tags$td(style="font-weight:500;", g$name),
                tags$td(style="color:var(--ink-light);font-size:.8rem;",
                        substr(gsid, nchar(gsid)-5, nchar(gsid))),
                tags$td(
                    tags$div(style="display:flex;gap:.5rem;flex-wrap:wrap;",
                             actionButton(paste0("ren_", gsid), "Rename", class="btn btn-ghost btn-sm",
                                          onclick=paste0("Shiny.setInputValue('rename_target_sid','",gsid,"',{priority:'event'})")),
                             actionButton(paste0("kck_", gsid), "Remove", class="btn btn-danger btn-sm",
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
        cid   <- admin_cid(); gsid <- rename_target(); req(cid, gsid)
        nm    <- trimws(input$rename_new_name)
        if (nchar(nm) == 0) { showNotification("Name cannot be empty.", type="error"); return() }
        old   <- list_guests(cid)[[gsid]]$name %||% "Guest"
        update_guest_name(cid, gsid, nm)
        add_activity(cid, paste0("<strong>",old,"</strong> renamed to <strong>",nm,"</strong>"))
        rename_target(NULL); bump(cid); showNotification(paste("Renamed to:", nm))
    })
    observeEvent(input$kick_sid, {
        cid  <- admin_cid(); gsid <- input$kick_sid; req(cid, gsid)
        nm   <- list_guests(cid)[[gsid]]$name %||% "Guest"
        kt   <- get_kick_trig(gsid); kt(TRUE)
        remove_guest(cid, gsid)
        add_activity(cid, paste0("<strong>",nm,"</strong> was removed from the room"))
        bump(cid); showNotification(paste("Removed:", nm), type="warning")
    })

    # ── Export CSV ────────────────────────────────────────────────────────────
    output$export_csv <- downloadHandler(
        filename = function() paste0("chorus_", admin_cid() %||% "export", "_", format(Sys.Date(),"%Y%m%d"), ".csv"),
        content  = function(file) {
            cid <- admin_cid(); req(cid)
            e   <- get_conf(cid); req(e)
            qs  <- as.list(e$questions)
            if (length(qs) == 0) { write.csv(data.frame(Message="No data"), file, row.names=FALSE); return() }
            all_rows <- do.call(rbind, lapply(names(qs), function(qid) {
                q <- qs[[qid]]; r <- q$responses
                if (nrow(r) == 0) return(NULL)
                data.frame(Conference=cid, Question=q$text, Answer=r$Answer, Votes=r$Votes,
                           Submitter=r$Submitter %||% "",
                           AdminSuggested=if(!is.null(r$IsAdmin)) r$IsAdmin else FALSE,
                           stringsAsFactors=FALSE)
            }))
            if (is.null(all_rows)) all_rows <- data.frame(Message="No responses yet")
            write.csv(all_rows, file, row.names=FALSE)
        }
    )

    # ── Guest: question selector ──────────────────────────────────────────────
    output$question_ui_guest <- renderUI({
        qs <- qs_g()
        if (length(qs) == 0)
            return(tags$p(style="color:var(--ink-light);font-size:.95rem;padding:.5rem 0;",
                          "Waiting for the organizer to post a question…"))
        selectInput("question_id_guest", NULL,
                    choices = setNames(names(qs), sapply(qs, `[[`, "text")),
                    selected = guest_qsel())
    })
    observeEvent(input$question_id_guest, { guest_qsel(input$question_id_guest) })

    # ── Guest: answer cards ───────────────────────────────────────────────────
    output$guest_answer_section <- renderUI({
        trig_g()
        cid <- guest_cid(); qid <- guest_qsel(); req(cid, qid)
        if (is_kicked()) return(NULL)
        e <- get_conf(cid); req(e)
        if (!exists(qid, envir = e$questions, inherits = FALSE)) return(NULL)
        q         <- get(qid, envir = e$questions, inherits = FALSE)
        responses <- q$responses
        s         <- e$settings
        locked    <- isTRUE(q$locked)

        va         <- user_votes()
        total_cast <- sum(unlist(lapply(va, function(v) sum(v, na.rm=TRUE))), na.rm=TRUE)
        q_cast     <- sum(va[[qid]], na.rm=TRUE)

        total_ok  <- within_total_limit(total_cast, s$max_votes_total)
        per_q_ok  <- within_q_limit(q_cast, s$max_votes_per_q)
        at_limit  <- !total_ok || !per_q_ok

        votes_by_user <- va[[qid]]
        if (is.null(votes_by_user)) votes_by_user <- setNames(integer(), character())
        total_all_votes <- if (nrow(responses) > 0) sum(responses$Votes) else 0L

        # Suggest box
        suggest_box <- if (locked) {
            tags$div(style="margin-bottom:1.5rem;",
                     tags$div(class="lock-banner",
                              tags$span(class="lock-icon","🔒"),
                              "The organizer has locked new suggestions for this question."
                     )
            )
        } else {
            tags$div(style="margin-bottom:1.5rem;",
                     tags$div(class="sec-eye","Suggest an answer"),
                     tags$div(style="display:flex;gap:.75rem;align-items:flex-end;",
                              tags$div(style="flex:1;",
                                       textInput("answer_text", NULL, placeholder="Type your answer here…")),
                              actionButton("submit_answer","Submit", class="btn btn-sage",
                                           style="margin-bottom:1rem;flex-shrink:0;")
                     )
            )
        }

        if (nrow(responses) == 0)
            return(tagList(suggest_box,
                           tags$p(style="color:var(--ink-light);font-size:.95rem;",
                                  if (locked) "No answers yet." else "No answers yet — be the first!")))

        responses <- responses[order(-responses$Votes), ]

        cards <- lapply(seq_len(nrow(responses)), function(i) {
            aid       <- as.character(responses$ID[i])
            my_v      <- if (!is.na(votes_by_user[aid])) votes_by_user[aid] else 0L
            has_voted <- my_v > 0L
            pct       <- if (total_all_votes > 0) round(responses$Votes[i] / total_all_votes * 100) else 0L
            is_adm    <- isTRUE(responses$IsAdmin[i])

            card_cls  <- paste0("answer-card",
                                if (has_voted) " voted" else "",
                                if (is_adm)   " admin-card" else "")

            sub_lbl   <- if (is_adm) "★ Organizer"
            else if (!is.null(responses$Submitter) &&
                     !is.na(responses$Submitter[i]) &&
                     nchar(responses$Submitter[i]) > 0) paste0("Suggested by ", responses$Submitter[i])
            else ""
            vote_lbl  <- paste0(responses$Votes[i], " vote", if(responses$Votes[i]!=1)"s" else "",
                                if(pct>0) paste0(" (", pct, "%)") else "")

            minus_btn <- if (has_voted)
                actionButton(paste0("m_",aid),"−", class="vbtn vbtn-minus",
                             onclick=paste0("Shiny.setInputValue('vote_action',{aid:'",aid,"',qid:'",qid,"',dir:-1},{priority:'event'})"))
            else NULL

            # Can add a vote?
            can_plus  <- !at_limit || has_voted
            plus_btn  <- if (can_plus)
                actionButton(paste0("p_",aid),"+", class="vbtn vbtn-plus",
                             onclick=paste0("Shiny.setInputValue('vote_action',{aid:'",aid,"',qid:'",qid,"',dir:1},{priority:'event'})"))
            else
                tags$button("+", class="vbtn vbtn-plus", disabled=NA,
                            style="opacity:.3;cursor:not-allowed;")

            tags$div(class=card_cls,
                     tags$div(class="answer-bar-bg",
                              style=paste0("width:",pct,"%;",if(pct==0)"display:none;" else "")),
                     tags$div(class="answer-content",
                              tags$div(class="answer-text", responses$Answer[i],
                                       if (is_adm) tags$span(class="admin-tag","Organizer") else NULL
                              ),
                              tags$div(class="answer-meta",
                                       if(nchar(sub_lbl)>0) sub_lbl else "",
                                       if(nchar(sub_lbl)>0) " · " else "",
                                       vote_lbl
                              )
                     ),
                     tags$div(class="answer-vote-controls", minus_btn,
                              tags$div(class="vote-count", responses$Votes[i]), plus_btn)
            )
        })

        msg_ui <- if (nchar(vote_msg()) > 0) tags$div(class="vote-msg", vote_msg()) else NULL

        tagList(
            suggest_box,
            tags$div(class="sec-eye","Vote on answers"),
            tags$div(class="answer-list", cards),
            msg_ui
        )
    })

    # ── Vote action ───────────────────────────────────────────────────────────
    observeEvent(input$vote_action, {
        if (is_kicked()) return()
        va       <- input$vote_action
        cid      <- guest_cid(); req(cid)
        qid      <- va$qid; aid_char <- as.character(va$aid); dir <- as.integer(va$dir)
        req(!is.null(qid), !is.null(aid_char))

        e   <- get_conf(cid); req(e)
        q   <- get(qid, envir = e$questions, inherits = FALSE)
        s   <- e$settings
        rs  <- q$responses
        ri  <- which(as.character(rs$ID) == aid_char)
        if (length(ri) == 0) return()

        va_all <- user_votes(); votes <- va_all[[qid]]
        if (is.null(votes)) votes <- setNames(integer(), character())
        cur_my  <- if (!is.na(votes[aid_char])) votes[aid_char] else 0L

        # Compute totals
        total_cast <- sum(unlist(lapply(va_all, function(v) sum(v, na.rm=TRUE))), na.rm=TRUE)
        q_cast     <- sum(votes, na.rm=TRUE)

        nm <- guest_name(); if (nchar(nm)==0) nm <- "Someone"

        if (dir == 1L) {
            if (!within_total_limit(total_cast, s$max_votes_total)) {
                vote_msg(if(is.infinite(s$max_votes_total)) "" else
                    paste0("Total vote limit reached (", s$max_votes_total, " across all questions)")); return()
            }
            if (!within_q_limit(q_cast, s$max_votes_per_q)) {
                vote_msg(if(is.infinite(s$max_votes_per_q)) "" else
                    paste0("Vote limit reached for this question (", s$max_votes_per_q, " max)")); return()
            }
            rs$Votes[ri]    <- rs$Votes[ri] + 1L
            votes[aid_char] <- cur_my + 1L
            add_activity(cid, paste0("<strong>",nm,"</strong> voted for <em>",rs$Answer[ri],"</em>"))
        } else {
            if (cur_my <= 0L) return()
            rs$Votes[ri]    <- max(0L, rs$Votes[ri] - 1L)
            votes[aid_char] <- cur_my - 1L
            add_activity(cid, paste0("<strong>",nm,"</strong> removed a vote from <em>",rs$Answer[ri],"</em>"))
        }

        q$responses   <- rs
        assign(qid, q, envir = e$questions)
        va_all[[qid]] <- votes
        user_votes(va_all)
        vote_msg(""); bump(cid)
    })

    # ── Guest: submit answer ──────────────────────────────────────────────────
    observeEvent(input$submit_answer, {
        if (is_kicked()) return()
        cid <- guest_cid(); qid <- guest_qsel()
        ans <- trimws(input$answer_text); req(cid, qid, nchar(ans) > 0)
        e   <- get_conf(cid)
        q   <- get(qid, envir = e$questions, inherits = FALSE)
        if (isTRUE(q$locked)) {
            showNotification("Suggestions are locked for this question.", type="warning"); return()
        }
        new_id <- if (nrow(q$responses)==0) 1L else max(q$responses$ID) + 1L
        nm <- guest_name(); if (nchar(nm)==0) nm <- "A guest"
        q$responses <- rbind(q$responses,
                             data.frame(ID=new_id, Answer=ans, Votes=0L, Submitter=nm, IsAdmin=FALSE,
                                        stringsAsFactors=FALSE))
        assign(qid, q, envir=e$questions)
        updateTextInput(session, "answer_text", value="")
        add_activity(cid, paste0("<strong>",nm,"</strong> suggested: <em>",ans,"</em>"))
        bump(cid)
    })

    # ── Activity feeds ────────────────────────────────────────────────────────
    make_feed <- function(cid) {
        if (is.null(cid) || !exists(cid, envir=store$activity_log, inherits=FALSE))
            return(tags$div(class="activity-feed",
                            tags$div(class="activity-empty","No activity yet.")))
        log <- get(cid, envir=store$activity_log, inherits=FALSE)
        if (length(log)==0)
            return(tags$div(class="activity-feed",
                            tags$div(class="activity-empty","No activity yet.")))
        tags$div(class="activity-feed",
                 lapply(log, function(ev)
                     tags$div(class="activity-item",
                              tags$span(class="activity-ts", ev$ts),
                              tags$span(class="activity-msg", HTML(ev$msg))
                     )
                 )
        )
    }
    output$activity_feed_guest <- renderUI({ trig_g(); make_feed(guest_cid()) })
    output$activity_feed_admin <- renderUI({ trig_a(); make_feed(admin_cid()) })
}

shinyApp(ui, server)