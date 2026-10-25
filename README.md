# Multi-Asset Portfolio Studio (HTML/JavaScript + R-Shiny Backend)

Institutionelle Fachanwendung zur Beurteilung strategischer Asset-Allokationen (SAAs) und Währungsabsicherungen aus Perspektive von **CHF-**, **EUR-** und **USD-Investoren**, basierend auf einer vierstufigen Architektur (eigenständige HTML/CSS/ES-Module-Oberfläche ohne Bootstrap/bslib, R-Shiny-Protokolladapter, deterministische reine R-Engine und versionierte Daten-/Konfigurationsartefakte).

---

## 1. Start der Anwendung und Ausführung der Tests

### Anwendung starten
```powershell
Rscript -e "shiny::runApp('app.R', port = 3838, launch.browser = TRUE)"
```

### Automatisierte Tests ausführen
- **R-Unit- und Integrationstests (`testthat`)**:
  ```powershell
  Rscript -e "testthat::test_dir('tests/testthat', reporter = 'summary')"
  ```
- **Frontend-Store- und Matrix-Preview-Tests (`Node.js`)**:
  ```powershell
  node tests/js/test_store_preview.mjs
  ```
- **Reine R-Engine-Verifikation über alle drei Basiswährungen (`CHF`, `EUR`, `USD`)**:
  ```powershell
  Rscript data/demo/verify_engine.R
  ```

---

## 2. Verbindliche Vierschichten-Architektur

1. **HTML/CSS/JS-Darstellung (`www/`)**:
   - Individuelles HTML-Template (`www/index.html`) eingebunden über `shiny::htmlTemplate("www/index.html")` mit `{{ headContent() }}`.
   - Kein Bootstrap, kein `bslib`, kein `shinydashboard`, kein `htmlwidgets`-Zwang.
   - Zentraler Frontend-Store (`www/js/store.js`) mit unmittelbarer Client-Vorschau ($a^\top \Sigma_B a$) aus der bereits empfangenen autoritativen Kovarianzmatrix und entprellter R-Prüfung (`www/js/shiny-bridge.js`).
   - Lokal versioniertes **Apache ECharts 5.5.1** (`www/vendor/echarts.min.js`) mit `ResizeObserver` und Bedarfs-Rendering (`www/js/charts/echarts-manager.js`).
2. **Shiny-Adapter (`app.R`, `R/server_commands.R`, `R/session_state.R`, `R/cache_jobs.R`)**:
   - Protokoll v1 über `Shiny.setInputValue('app_command', ..., { priority: 'event' })` und `session$sendCustomMessage('app_event', envelope)`.
   - Mehrstufiger `cachem`-Cache (Import, FX/Returns, Risikobasis/Kovarianz, Portfolioauswertung, Renditeszenarien) mit strikter Trennung zwischen geteiltem Hausdatensatz-Cache und privatem Sitzungscache.
   - Nichtblockierende Hintergrundjobs (`ExtendedTask` + `promises::future_promise`) für Walk-Forward- und Block-Bootstrap-Validierung mit Stale-Job-Schutz.
3. **Reine R-Engine (`R/*.R`)**:
   - Deterministische, zustandslose Funktionen für Import, Metadatenvalidierung, FX-Triangulation, Return-Konstruktion, gemeinsame Risikobasis $X_B = [u^B, z^B]$, Kovarianzschätzung, Exposure-/Hedge-Auflösung, Portfoliovergleich und 5-Jahres-Szenariopfade.
4. **Daten & Artefakte (`Data.xlsx`, `config/defaults.json`, `config/metadata-schema.json`)**:
   - Konsolidierte Metadaten, strukturierte Policy-Konfigurationen und kryptografisch gehashte Export-Bundles (`XLSX`, `JSON`, `CSV`, `RDS`).

---

## 3. Dokumentierte Fachlogik, Annahmen und Modellgrenzen

### FX-Normalisierung und 1M-Forward-Hedge
- **Interner Numeraire**: Alle Spot- und 1M-Forward-Reihen werden nach Skalierung (`Quote_Scale`) und allfälliger Points-Umrechnung (`Forward_Type = POINTS`) zunächst in USD je Währungseinheit ($V_{C,t}$, $G_{C,t}$ mit $V_{\text{USD},t} = G_{\text{USD},t} = 1$) überführt und erst auf Niveau-Ebene in die gewählte Basiswährung $B$ trianguliert:
  $$S_t^{(B/C)} = \frac{V_{C,t}}{V_{B,t}}, \quad F_t^{(B/C)} = \frac{G_{C,t}}{G_{B,t}}$$
- **1M-Kalendernäherung**: Da exakte Fälligkeitstage historischer 1M-Forwards nicht vorliegen, dient der 1M-Monatsend-Forward $F_{t-1}^{(B/C)}$ als dokumentierte Näherung für die Monatsendperiode $t-1 \to t$:
  $$z_{C,t}^B = \frac{F_{t-1}^{(B/C)} - S_t^{(B/C)}}{S_{t-1}^{(B/C)}} = \underbrace{\left(\frac{F_{t-1}^{(B/C)}}{S_{t-1}^{(B/C)}} - 1\right)}_{\text{carry}} + \underbrace{\left(-\left(\frac{S_t^{(B/C)}}{S_{t-1}^{(B/C)}} - 1\right)\right)}_{\text{spot\_offset}}$$
- **Kreuzterm**: Für ein abgesichertes Asset gilt $r_i(h) = u_i^B + h \cdot z_C^B$. Bei Vollhedging ($h=1$) verbleibt der lokale Rendite-/FX-Kreuzterm $r_{L,i,t} \cdot fx_{C,t}^B$ im Portfolio.
- **GBP-Spot-Konvention**: Der Code und die Kursrichtung für GBP-Spot werden dynamisch aus der Datei-/Metadatenkonfiguration gelesen. Liegt in `Data.xlsx` noch der historische BOE-Snapshot-Code `USSTBOE` (`FOREIGN_PER_QUOTE`, GBP je USD) vor, wird dieser explizit validiert und invertiert; wird die Reihe auf USD je GBP (`QUOTE_PER_FOREIGN`) oder einen neuen Code umgestellt, übernimmt die Engine den neuen Code und die neue Richtung automatisch.
- **Fehlender JPY-1M-Forward**: Im aktuellen Datensatz fehlt eine aktive `JPY`-1M-Forward-Reihe. Ungesicherte Japan-Positionen (`TOKYOSE(RI)`, $h=0$) sind uneingeschränkt nutzbar; jede Konfiguration, die ein positives JPY-Hedge-Nominal ($g_{\text{JPY}} > 0$) anfordert, wird gezielt abgelehnt bzw. im Klassen-Hedge mit expliziter Warnung ausgenommen.
- **EM-Aktien (`MSEMKF$(RI)`)**: Werden aus ihrer Indexwährung USD in die Basiswährung $B$ umgerechnet, tragen aber das Exposure-Label `EMEQ` (`Hedge_Allowed = FALSE`) und fliessen nie in den hedgefähigen USD-Nenner ein.
- **Gold & Rohstoffe (`GSGCTOT`, `DJUBSTR`)**: Sind als `FUTURES_TR` (inkl. Collateral- und Roll-Ertrag) modelliert, nicht als physisches Gold. Ein USD-Hedge steuert ausschliesslich das kontraktuelle Index-Nominal.
- **EUR-Cash-Proxy vor 1999 (`ECWGM1M`)**: Darf über die strukturierte Policy `allow_pre_1999_proxy` als bewusst gewählter historischer Proxy (vermutete DEM-/Euro-Mark-Herkunft) bis `1998-12-31` genutzt werden; ab 1999 entspricht die Reihe EUR. Bei Excess-Return-Analysen wird der Basiswährungs-Cashreturn genau **einmal** von jedem Assetreturn $u_{i,t}^B$ abgezogen und **nie** von den Hedgekomponenten $z_{C,t}^B$.

### Noch nicht enthaltene spätere Ausbaustufen (gemäss Phasenplan)
- **Numerische SAA-Optimierung**: Gemäss Abschnitt 1 ("Optimierung von SAAs ist eine spätere Funktion") liegt der Fokus auf exakter Bewertung, Risikozurechnung und Mehr-Portfolio-Vergleich.
- **Konvexes Matrix-Ensemble**: Einzelmodelle (`Sample`, `Shrinkage Vollhistorie`, `Shrinkage 120M`, `EWMA 24M`, `Faktor-Momentenmodell`, `Krisen-Stress`) stehen bereit und werden im Tab `5. SAA-Vergleich & TE` nebeneinander gegenübergestellt; ein konvexes Mischgewicht über kompatible Matrizen kann darauf direkt aufsetzen.
- **Stochastische Mehrjahres-Quantile**: Die 5-Jahres-Renditeszenarien sind gemäss Abschnitt 8 deterministische Rebalancingpfade (`Szenariorenditen`), ohne unfundierte Verlustwahrscheinlichkeiten.

---

## 4. Gemessene Performance-Budgets (Zielumgebung)

Gemessen auf lokalem Windows-System mit **R 4.5.0 (ucrt)** und `Data.xlsx` (24 Zeitreihen, 442 Monate `1989-12` bis `2026-09`):

| Messgrösse | Gemessener Wert | Zielbudget |
|---|---|---|
| Kaltimport & Validierung (`Data.xlsx`, inkl. SHA-256) | **~250 ms** | < 1000 ms |
| Kovarianzschätzung (`shrinkage_long` inkl. PSD-Diagnostik) | **~130 ms** | < 500 ms |
| Warm-Portfolioauswertung (`portfolio.evaluate` inkl.Envelope) | **~190 ms** | < 300 ms |
| Synthetischer Stresstest (30 Assets + 10 Hedges × 600 Monate) | **< 45 ms** | < 300 ms |
| JSON-Payload bei `portfolio.evaluate` (inkrementell) | **~102.5 KB** | Kompakt, ohne Rohhistorie |
| Neue Kovarianzschätzungen bei Slider-/Hedgebewegung | **0** | Strikt 0 (mit Zähler getestet) |
