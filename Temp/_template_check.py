#!/usr/bin/env python3
# Quality-check scan: among the 18 DISPLAYED candidates, find per-question
# submissions that are template-only / no-effort (unchanged stencil).
import csv, re, os

ROOT = "/mnt/c/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring"
MAIN = os.path.join(ROOT, "2input_data/01studentlab_scores/main")
RESUME = os.path.join(ROOT, "3output_data/Main/student_data/student_matching/student_resume.csv")
XWALK = os.path.join(MAIN, "student_code_crosswalk.csv")

# --- 18 displayed candidates: resume_index is non-blank in student_resume.csv ---
displayed = {}   # name -> (resume_index, test_case)
with open(RESUME, newline='', encoding='utf-8-sig') as f:
    for r in csv.DictReader(f):
        if r["resume_index"].strip():
            displayed[r["name"].strip()] = (int(r["resume_index"]), float(r["test_case"]))

# --- crosswalk: name -> Q1 code, Q2 code ---
codes = {}
with open(XWALK, newline='', encoding='utf-8-sig') as f:
    for r in csv.DictReader(f):
        codes[r["name"].strip()] = (r["Q1"].strip(), r["Q2"].strip())

def read_code(q, num):
    p = os.path.join(MAIN, q.upper(), f"{q}_{num}.txt")
    if not os.path.exists(p): return None, p
    with open(p, encoding='utf-8', errors='replace') as fh:
        return fh.read(), p

# --- extract the solution body (between the function signature and the stencil/main boilerplate) ---
STENCIL_MARKERS = ["Stencil Main", "stencil main", "# Read input", "public static void main",
                   "Read input lists", "###"]
def solution_body(src):
    if src is None: return None
    lines = src.splitlines()
    # find the solution function def line (getContentChildren / candidate solution fn)
    start = None
    for i, ln in enumerate(lines):
        if re.search(r'\b(getContentChildren|def\s+\w+|public\s+static\s+int\s+\w+)\b', ln) and 'main' not in ln:
            start = i; break
    if start is None: start = 0
    # find where the stencil/boilerplate begins, after start
    end = len(lines)
    for j in range(start+1, len(lines)):
        if any(m in lines[j] for m in STENCIL_MARKERS):
            end = j; break
    body = lines[start+1:end]
    return body

def is_no_effort(src):
    body = solution_body(src)
    if body is None: return None, "MISSING FILE"
    meaningful = []
    for ln in body:
        s = ln.strip()
        if not s: continue
        if s.startswith('#') or s.startswith('//'): continue        # comment
        if s in ('return 0', 'return', 'pass', 'return 0;', 'return;'): continue  # stencil default
        if re.fullmatch(r'(your solution|your code here)', s, re.I): continue
        meaningful.append(s)
    return (len(meaningful) == 0), meaningful

print(f"{'idx':>3} {'name':22} {'test_case':>9}  Q1            Q2")
print("-"*78)
rows = sorted(displayed.items(), key=lambda kv: kv[1][0])
flagged = []
for name, (idx, tc) in rows:
    q1c, q2c = codes.get(name, ('?','?'))
    out = {}
    for q, c in (('q1', q1c), ('q2', q2c)):
        src, path = read_code(q, c)
        ne, info = is_no_effort(src)
        if ne is None:
            out[q] = "NA(nofile)"
        elif ne:
            out[q] = f"** TEMPLATE-ONLY (code {c}) **"
            flagged.append((idx, name, q.upper(), c, tc))
        else:
            out[q] = f"effort ({len(info)} ln, code {c})"
    print(f"{idx:>3} {name:22} {tc:9.2f}  {out.get('q1',''):28} {out.get('q2','')}")

print("\n=== FLAGGED template-only submissions among the 18 displayed ===")
if not flagged:
    print("  none")
for idx, name, q, c, tc in sorted(flagged):
    print(f"  resume_index {idx:>2}  {name:22} {q} (code {c})   overall test_case={tc:.2f}")
