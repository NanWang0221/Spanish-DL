#!/usr/bin/env python3
"""
Cue analysis for the d-t VOT-continuum stimuli (dill/din/dip * VOT step).

Adapts Chodroff (2017) cueAnalysis_engCVC.praat to stimuli that have NO
TextGrids. The original reads stop/vowel boundaries from *_autovot.TextGrid;
those don't exist here, so:

  VOT       -> from the filename value (the designed continuum step, ms).
               negative = voicing lead / prevoicing; positive = long-lag.
  duration  -> total file duration; plus vowel-nucleus duration.
  F0        -> Praat 'To Pitch... 0.0 75 600' (floor 75, ceiling 600),
               exactly as in the script. Vowel onset is located by an
               intensity threshold (peak-10 dB, sustained >=25 ms) so the
               same rule works for /l/, /n/ and /p/ codas and for both
               prevoiced and aspirated tokens. F0 is then sampled every 5 ms
               over the first 50 ms of the vowel (f0_1..f0_10, Chodroff-style)
               and summarized over the whole vowel nucleus.
"""
import glob, os, re, csv
import numpy as np
import parselmouth
from parselmouth.praat import call

BASE = "/mnt/user-data/uploads/Spanish-DL/stimuli/d-t"
SUBDIR = "!final VOT continuum_selected_70dBnormed"
PAIRS = [("dill_till", "dill"), ("din_tin", "din"), ("dip_tip", "dip")]

F0_FLOOR, F0_CEIL = 75.0, 600.0
STEP = 0.005
DROP_DB = 10.0        # vowel = region within 10 dB of the intensity peak
MIN_RUN = 0.025       # sustained >= 25 ms


def vot_from_name(stem, prefix):
    m = re.match(re.escape(prefix) + r"_([+-]?\d+)$", stem)
    return int(m.group(1)) if m else None


def vowel_region(snd):
    """Locate the vocalic nucleus via an intensity threshold (peak - DROP_DB),
    returning (onset_s, end_s) of the longest sustained loud run."""
    inten = snd.to_intensity(minimum_pitch=100)
    imax = call(inten, "Get maximum", 0, 0, "Parabolic")
    thr = imax - DROP_DB
    grid = np.arange(snd.xmin, snd.xmax, STEP)
    mask = []
    for t in grid:
        v = call(inten, "Get value at time", t, "Cubic")
        mask.append((not np.isnan(v)) and v >= thr)
    mask = np.array(mask)
    # longest run of True
    best_len, best_s, best_e = 0, None, None
    i = 0
    while i < len(mask):
        if mask[i]:
            j = i
            while j < len(mask) and mask[j]:
                j += 1
            if (j - i) > best_len:
                best_len, best_s, best_e = j - i, i, j - 1
            i = j
        else:
            i += 1
    if best_s is None or (grid[best_e] - grid[best_s]) < MIN_RUN:
        return None, None
    return float(grid[best_s]), float(grid[best_e])


def rnd(x, n=2):
    return round(x, n) if (x == x) else ""   # x==x is False for NaN


rows = []
for pairname, prefix in PAIRS:
    folder = os.path.join(BASE, pairname, SUBDIR)
    files = glob.glob(os.path.join(folder, prefix + "_*.wav"))
    for path in files:
        stem = os.path.splitext(os.path.basename(path))[0]
        vot = vot_from_name(stem, prefix)
        snd = parselmouth.Sound(path)
        total_ms = (snd.xmax - snd.xmin) * 1000.0
        pitch = call(snd, "To Pitch", 0.0, F0_FLOOR, F0_CEIL)

        v_on, v_off = vowel_region(snd)
        if v_on is None:
            vowel_ms = float("nan")
            onset_vals = [float("nan")] * 10
            f0_on = f0_mean = f0_med = f0_min = f0_max = f0_sd = float("nan")
        else:
            vowel_ms = (v_off - v_on) * 1000.0
            onset_vals = []
            for p in range(1, 11):
                v = call(pitch, "Get value at time", v_on + p * STEP, "Hertz", "Linear")
                onset_vals.append(v if not np.isnan(v) else float("nan"))
            ov = np.array([x for x in onset_vals if x == x])
            f0_on = float(np.mean(ov)) if ov.size else float("nan")
            f0_mean = call(pitch, "Get mean", v_on, v_off, "Hertz")
            f0_med = call(pitch, "Get quantile", v_on, v_off, 0.5, "Hertz")
            f0_min = call(pitch, "Get minimum", v_on, v_off, "Hertz", "Parabolic")
            f0_max = call(pitch, "Get maximum", v_on, v_off, "Hertz", "Parabolic")
            f0_sd = call(pitch, "Get standard deviation", v_on, v_off, "Hertz")

        row = {
            "pair": pairname, "stem": stem, "vot_ms": vot,
            "file_duration_ms": rnd(total_ms, 2),
            "vowel_onset_s": rnd(v_on, 4) if v_on is not None else "",
            "vowel_dur_ms": rnd(vowel_ms, 2),
            "f0_onset_mean_hz": rnd(f0_on),
            "f0_mean_hz": rnd(f0_mean), "f0_median_hz": rnd(f0_med),
            "f0_min_hz": rnd(f0_min), "f0_max_hz": rnd(f0_max), "f0_sd_hz": rnd(f0_sd),
        }
        for p in range(1, 11):
            row[f"f0_{p}"] = rnd(onset_vals[p - 1])
        rows.append(row)

rows.sort(key=lambda r: (r["pair"], r["vot_ms"] if r["vot_ms"] is not None else 0))

cols = ["pair", "stem", "vot_ms", "file_duration_ms", "vowel_onset_s", "vowel_dur_ms",
        "f0_onset_mean_hz", "f0_mean_hz", "f0_median_hz", "f0_min_hz", "f0_max_hz",
        "f0_sd_hz"] + [f"f0_{p}" for p in range(1, 11)]

out = "/tmp/cueAnalysis_dt_continuum.csv"
with open(out, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=cols)
    w.writeheader(); w.writerows(rows)

print(f"Wrote {len(rows)} rows -> {out}\n")
hdr = f"{'stem':11}{'VOT':>5}{'file_ms':>9}{'vwl_ms':>8}{'f0_on':>7}{'f0_mean':>8}"
for pr in ("dill_till", "din_tin", "dip_tip"):
    print("==", pr)
    print(hdr)
    for r in [x for x in rows if x["pair"] == pr]:
        print(f"{r['stem']:11}{r['vot_ms']:>5}{r['file_duration_ms']:>9}{r['vowel_dur_ms']:>8}{r['f0_onset_mean_hz']:>7}{r['f0_mean_hz']:>8}")
