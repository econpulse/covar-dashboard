# Implementierungsplan für eine Coding-KI
# Multi-Asset Portfolio Studio: HTML/JavaScript mit R-Shiny-Backend

## 1. Ziel und verbindliche Architektur

Entwickle eine lokal und auf einem Shiny-Server betreibbare Anwendung, mit der institutionelle Anwender mehrere strategische Asset-Allokationen aus Perspektive von CHF-, EUR- und USD-Investoren beurteilen. Kombiniere zukunftsgerichtete Renditeannahmen mit mehreren historischen bzw. modellgestützten Risikomatrizen. Anwender steuern Kapitalgewichte sowie Währungsabsicherungen und vergleichen die resultierenden Rendite-/Risikoeigenschaften.

Verwende eine vollständig individuell gestaltete HTML/CSS/JavaScript-Oberfläche. R-Shiny übernimmt Sitzung, Kommunikation, Validierung und Berechnungen. Kein bslib-, Bootstrap-, shinydashboard-, AdminLTE- oder Standard-htmlwidget-Layout. Kein Python-Backend. Kein React-/Node-Laufzeitserver nötig. Keine unabhängige REST-Schicht, solange sie keinen belegten Nutzen hat.

Das Ziel ist eine kleine, schnell reagierende Fachanwendung, keine Ansammlung aller verfügbaren Optimierer. Priorität: korrekte Daten- und Währungslogik, nachvollziehbare Risikoschätzung, nachvollziehbare Portfoliovergleiche, dann Erweiterungen. Optimierung von SAAs ist eine spätere Funktion.

Trenne vier Schichten:

1. HTML/CSS/JS: Darstellung, Navigation, Eingabestatus und leichte Vorschau.
2. Shiny-Adapter: Nachrichten, Sitzung, Jobs, Cache und autoritative Validierung.
3. Reine R-Engine: deterministische Funktionen ohne Shiny-Abhängigkeit.
4. Daten/Artefakte: Rohdaten, versionierte Metadaten, Konfiguration und Ergebnisse.

Die R-Engine ist die fachliche Referenz. JS darf bereits empfangene Matrizen für unmittelbare Vorschauen nutzen; offizielle Ergebnisse/Exporte werden in R gerechnet und erhalten einen Konfigurationshash.

## 2. Festgelegter fachlicher Rahmen

- Rohdaten sind monatliche Niveaus, keine vorab berechneten Renditen.
- Assetreihen sind Total-Return-Indizes in ihrer gelieferten Währung.
- Spots und 1M-Forwards liegen gegenüber USD vor, jeweils per Monatsende.
- Tatsächliche Forward-Fälligkeiten sind nicht vorhanden. 1M wird als Näherung für die nächste Monatsendperiode verwendet. Nicht nach exakten Terms fragen und keine exakte Hedge-Replikation behaupten.
- Keine erwarteten Lücken innerhalb der Zeitreihen; unterschiedliche Historienanfänge sind zulässig. Echte Innenlücken bleiben Fehler/fehlende Beobachtungen, nicht null.
- Forwarddaten reichen vermutlich erst bis etwa 2000 zurück. Abdeckung aus Daten bestimmen, Datum nicht hardcoden.
- Basiswährungen: CHF, EUR, USD.
- EM-Aktien: gelieferter USD-TR-Index, Exposurelabel EMEQ, keine Absicherung. Nach gewählter Basiswährung umrechnen; USD-Rechenwährung ist kein USD-Hedge-Exposure.
- GBP-Spot wurde vom Nutzer auf USD je GBP umgestellt, passend zum GBP-Forward. Der neue Code ist aus der tatsächlichen Datei/Metadatenkonfiguration zu übernehmen. Der alte Snapshot-Code USSTBOE und seine alte Kursrichtung dürfen nicht automatisch weitergelten.
- Die EU-MARK-Deposit-Reihe darf als bewusst gewählter EUR-Cash-Proxy vor 1999 verwendet werden. Ab 1999 entspricht sie laut Nutzer der EUR-Reihe. Vermutete DEM-Herkunft als Annahme dokumentieren; nicht als damals existierende EUR-Anlage bezeichnen.
- Standard: Total-Return-Risikomodell. Excess Returns zusätzlich und optional.
- Aktienkapitalquoten 25/50/75 % dienen als optionale Profilvorlagen. Freie SAA-Gewichte bleiben möglich.

## 3. Darstellung ohne Shiny-Standarddesign

Nutze ein HTML-Template mit `shiny::htmlTemplate()` und `{{ headContent() }}` für die grundlegenden Shiny-Abhängigkeiten. Keine `bootstrapLib()`, `fluidPage()`, `dashboardPage()` oder Bootstrap-Komponenten. App-JavaScript und Styles liegen in `www/`; verwende ES-Module, soweit mit den konkret eingesetzten Shiny-Abhängigkeiten getestet. Shiny initialisiert sich über seine eigenen Abhängigkeiten; Handshake und Eventregistrierung müssen nachweislich funktionieren.

Die UI enthält native HTML-Inputs und eigene Komponenten. Kommunikation:

```javascript
Shiny.setInputValue('app_command', {
  protocol_version: 1,
  request_id: crypto.randomUUID(),
  type: 'portfolio.evaluate',
  context: { dataset_id, basis_id, model_id, state_version },
  payload: { weights, hedge_config, expectations_id }
}, { priority: 'event' });

Shiny.addCustomMessageHandler('app_event', message => {
  // Nachricht prüfen und anhand request_id / Kontext dem aktuellen Zustand zuordnen.
});
```

R antwortet mit `session$sendCustomMessage('app_event', envelope)`. Nachrichten als Objekte senden, nicht doppelt JSON-kodieren. Handler vor erstem Request registrieren; nach Shiny-Verbindung einen Bootstrap-Request senden. Verbindungsabbruch sichtbar machen, Änderungen bei Wiederverbindung abgleichen; Jobs nicht blind erneut auslösen.

Anforderungen an das Design:

- Aufgeräumte Arbeitsoberfläche, links kompakte Navigation, oben aktiver Portfolio-/Basiswährungskontext.
- Dezente Farben, klare Typografie, CSS Grid/Flexbox und Design Tokens; keine schweren dekorativen Effekte.
- Native Tabellen mit festen Identifikationsspalten; nur bei grossen Datenmengen virtualisieren/paginieren.
- Diagramme direkt in JavaScript, vorzugsweise Apache ECharts als lokal versioniertes Asset. Kein Plotly-/DT-/htmlwidget-Zwang. Chartdaten und fachliche Berechnungen trennen.
- Heatmaps nur bei Bedarf rendern, Chartinstanzen erhalten und Daten aktualisieren. ResizeObserver, Entsorgung bei Komponentenabbau, keine Listener-Duplikate.
- Fokus, Tastaturbedienung, Labels, Kontraste und verständliche Fehlermeldungen. Matrixzahlen auch tabellarisch zugänglich.
- Sichtbare Zustände: nicht berechnet, aktuell, Änderungen nicht angewendet, läuft, veraltet, fehlgeschlagen, nicht verfügbar.
- Änderungen an UI-Details oder Tabellenfiltern lösen keine R-Neuberechnung aus.

Upload: standardmässig ein serverseitig konfiguriertes Excel-/Parquet-Inputfile. Optionaler interaktiver Upload über dokumentierte Shiny-File-Input-Bindings mit individueller CSS-Gestaltung; Rohdateien nicht als riesige JSON/Base64-Nachrichten versenden. Downloads über `downloadHandler` bzw. dokumentierte Sitzungspfade, keine ungetesteten internen Shiny-Endpunkte.

## 4. Datenvertrag und konsolidierte Metadaten

Unterstütze den vorhandenen Excel-Wide-Import:

    Code | 31.12.1989 | 31.01.1990 | ...

Eine zusätzliche Name-Spalte kann vorhanden sein und wird beim Werteimport ignoriert. Die autoritativen Anzeigenamen kommen aus dem konsolidierten Metadatenblatt. Datumsspalten können Excel-Seriennummern oder Datumsheader sein; Konvention explizit erkennen/validieren. Codes als exakte Zeichenketten behandeln, insbesondere `$`, Klammern und Sonderzeichen. Keine automatische Namensbereinigung wie `make.names()`.

Interne normalisierte Rohdaten:

    date | Code | value

Relevante Felder der bereits entworfenen konsolidierten Metadaten:

    Code, Name, Series_Type, Value_Unit, Active, Review_Status,
    Asset_Class, Sub_Asset_Class, Region, Return_Currency, FX_Exposure,
    Hedge_Allowed, Default_Hedge_Ratio, TR_Convention, Already_Hedged,
    Existing_Hedge_Currency, Return_Structure,
    Foreign_Currency, Quote_Currency, Quote_Direction, Quote_Scale,
    Forward_Type, Points_Multiplier, Tenor,
    Cash_Currency, Rate_Convention, Day_Count,
    Spot_Reference_Code, Source, Notes

`Code` ist der Schlüssel einer Rohserie. Für MVP darf der TR-Code zugleich Asset-ID sein. Jede Liste/Matrix enthält zusätzlich explizite `asset_ids` bzw. `component_ids`; Reihenfolge nie aus Anzeigenamen ableiten.

Metadatenstand aktualisieren, ohne Nutzerkorrekturen zu überschreiben:

- GBP neue Spotrichtung USD je GBP, tatsächlichen neuen Code übernehmen.
- EUR-Cash-Proxy als Nutzerentscheidung zulassen. Optional Metadaten um `Proxy_Type`, `Proxy_Until`, `Proxy_Description` ergänzen, sauber versioniert; alternativ strukturierter Konfigurationsblock für diesen bekannten historischen Wechsel. Kein Freitext-Parsing von Notes für Geschäftslogik.
- Im Snapshot fehlt ein JPY-1M-Forward. Japan-Hedging bei fehlenden Forwarddaten sperren; ungesicherte Japanposition bleibt nutzbar.
- GSGCTOT ist laut Bezeichnung ein Gold-Futures-TR, kein physischer Goldindex. Renditemodell muss dazu passen.
- Name, `(RI)` und Indexhöhe beweisen weder Gross/Net noch sämtliche Währungskonventionen.
- Historienanfänge/-enden automatisch aus Daten berechnen, nicht manuell pflegen.

Bei mehreren passenden FX-Reihen explizite Auswahl verlangen/konfigurieren. Keine stille Auswahl des ersten Matches. `Active` steuert Universumsauswahl, nicht fachliche Datenfreigabe. Technische Fehler blockieren; bekannte Näherungen mit zulässiger Konfiguration warnen. Bereits in den gelieferten Index eingebaute Hedges im MVP nur bei klarer, unterstützter Behandlung verwenden; sonst erklärbar sperren.

## 5. FX-Normalisierung und Return-Konstruktion

Interner Numeraire:

    V_C,t = USD je Einheit C, V_USD,t = 1
    G_C,t = 1M-Forward: USD je Einheit C, G_USD,t = 1

Zuerst Skalierung und gegebenenfalls Forward Points korrekt in Outrights überführen, danach invertieren. Bei Points müssen Bezugsspot und Rohkursrichtung passen. Spottabelle und Forwardtabelle dürfen unterschiedliche Rohkursrichtungen besitzen. Für den korrigierten GBP-Datensatz stimmen sie nun überein.

Kreuzkurse für Basiswährung B:

    S_t^(B/C) = V_C,t / V_B,t
    F_t^(B/C) = G_C,t / G_B,t

Forwardkreuze nur aus Quotes desselben Monats und desselben 1M-Labels ableiten. USD-Legs sind Identitäten. B=C liefert Spot/Forward 1 und keinen Hedge. Provider-Fixing-Unterschiede und die unbekannten echten 1M-Fälligkeiten bleiben dokumentierte Näherungen. Triangulation zuerst für Niveaus, nie für bereits berechnete Währungsrenditen.

Lokale Monatsrendite:

    r_L,i,t = TR_i,t / TR_i,t-1 - 1

Ungesicherte Basiswährungsrendite:

    u_i,t^B = (1 + r_L,i,t) * S_t^(B/C) / S_t-1^(B/C) - 1

C ist die Publikationswährung des TR-Index. EM wird aus USD umgerechnet und bleibt unabhängig davon EMEQ.

Monatlicher vollständiger Forward-Hedge-PnL je Exposurewährung C:

    z_C,t^B = (F_t-1^(B/C) - S_t^(B/C)) / S_t-1^(B/C)

Zerlegung zu Audit-Zwecken:

    carry = F_t-1/S_t-1 - 1
    spot_offset = -(S_t/S_t-1 - 1)
    z = carry + spot_offset

Hedge auf Monatsanfangsbestand, fester Nominalbetrag innerhalb des Monats, Monatsendabrechnung, Reset im Folgemonat. Für ein Asset gilt r_i(h)=u_i+h*z_C. Der lokale Rendite-/FX-Kreuzterm bleibt bei Vollhedging erhalten. Keine doppelte Addition oder Subtraktion von Carry. Bid/Ask-/Transaktionskosten als separate deklarierte Annahme, nicht als implizites zweites Carry.

Bei fehlendem Vormonatswert keine Monatsrendite erzeugen. Ein Mehrmonatsabstand darf nicht als Einmonatsreturn erscheinen.

## 6. Gemeinsame Risikobasis je Basiswährung

Für ein gewähltes Analyseuniversum konstruiere:

    X_B = [u_asset1^B, ..., u_assetN^B, z_ccy1^B, ..., z_ccyK^B]
    Sigma_B = Cov(X_B)

Die Matrix enthält Renditen und Hedge-PnLs, keine FX-/Forward-/Zinsniveaus. Je Währung nur eine Hedgekomponente. EM bekommt keine eigene Hedgekomponente.

Die benötigten Hedgewährungen werden aus dem Analyseuniversum und der Hedge-Zulässigkeit abgeleitet, nicht aus der gerade gewählten Hedgequote. Sonst würde eine Slideränderung unerwartet die Modellbasis ändern. Hedgewährungen ohne Exposure im Universum gar nicht erst aufnehmen. Bereits enthaltene Nullgewichtsassets können im stabilen Arbeitsuniversum bleiben; deren spätere Gewichtänderung benötigt keine Neuschätzung.

Fehlender JPY-Forward darf nicht die gesamte ungesicherte Analyse blockieren: Komponentenverfügbarkeit separat verwalten. Matrix mit vorhandenen Hedgekomponenten berechnen; jede Konfiguration, die eine fehlende Komponente benötigt, gezielt ablehnen. Unterschiedliche Matrizen benötigen zur ehrlichen Gegenüberstellung einen gemeinsamen Zeitraum oder einen klar gekennzeichneten Zeitraumunterschied.

Je Basiswährung neu konstruieren/schätzen und separat cachen. Eine bereits geschrumpfte CHF-Matrix nicht als exakte EUR-/USD-Matrix transformieren. Gleiche Einstellungen und Vergleichsfenster verwenden; Shrinkage kann basisabhängige Unterschiede erzeugen.

### Risikosichten

Pflicht:
- Sample-Kovarianz als Diagnosebenchmark.
- Langfristiges Shrinkage-Modell auf belastbarer gemeinsamer Historie.
- Mittelfristiges Shrinkage-Modell, Startwert 120 Monate.
- EWMA mit dokumentierter Regularisierung, Startwert Halbwertszeit 24 Monate und maximal 120 Monate.

Später:
- PSD-konsistente Stressmodelle bzw. beobachtete Krisenfenster.
- Konvexes Ensemble kompatibler Matrizen.
- Strukturierter Faktoransatz als Alternative.

Schätzer und Target explizit dokumentieren. Klassische Ledoit-Wolf-Intensitäten nicht ungeprüft auf EWMA oder synthetisch vervollständigte Daten anwenden. Krisen nicht standardmässig durch MCD/Winsorisierung entfernen. Zu jeder Matrix: N, Window, Komponentenreihenfolge, Volatilitäten, Korrelationen, Eigenwerte, Kondition, Regularisierung, modellgestützte Anteile, Daten-/Metadatenhash und Version.

Primär monatliche Sigma. 12*Sigma als annualisierte Einmonats-Risikokonvention, nicht als exakte Jahres- oder Fünfjahresvarianz. Autokorrelation und Pfadabhängigkeit bei mehrjährigen Aussagen gesondert modellieren.

### Kurze Historien

MVP: vollständiger gemeinsamer Zeitraum mit klarer Abdeckungsanzeige. Lange ungesicherte Historie separat erhalten, da Forwards später beginnen. Kein Vergleich „ungesichert seit 1990“ gegen „gesichert seit 2000“ ohne Warnung.

Nächste Stufe: direkte gemeinsame Faktor-Momentenschätzung kurzer Assetreihen. Wenige wirtschaftlich zulässige Donoren, validierte Koeffizienten, residuale Varianz und soweit identifizierbar gemeinsame Residualkovarianz. Volle PSD-Struktur erhalten. Keine einzelne OLS-plus-Zufallsrauschen-Historie als wahre Vergangenheit verwenden. Proxy-Monate erhöhen nicht die echte Beobachtungszahl. Stationäre Exposures sind eine Annahme.

Keine Forward-Backcasts standardmässig. Zinsproxy/konstanter Carry vor Forwardverfügbarkeit nur als separat aktivierte Modellvariante. Jüngere Assets werden nicht allein deshalb zu langen „echten“ Historien gemacht.

## 7. Exposure- und Hedge-Engine

Trenne Kapitalgewicht w, hedgefähiges Exposure E, Hedgequote h und Forward-Nominal g. Für Single-Currency-Assets ist e_i,c=1 für die jeweilige Exposurewährung, sonst 0. EMEQ ist eine separate, nicht hedgefähige Kategorie. Bei Gold/Rohstoffen bezeichnet ein USD-Overlay das kontraktuelle Nominal, nicht sämtliche ökonomischen Währungsrisiken.

    E_c,vor = Sum_i w_i * e_i,c
    g_c = Sum_i w_i * e_i,c * h_i,c
    E_c,nach = E_c,vor - g_c

Hedgefähige und nicht hedgefähige Anteile getrennt führen. Cash/EMEQ/Inland nicht in fremdwährungsbezogene Nenner hineinrechnen. Bei Exposure 0 ist die effektive Quote n/a, nicht 0/0. Diese Gewichte beschreiben den Zeitpunkt unmittelbar nach dem Hedge-Reset, keinen konstanten untermonatlichen Bestand.

Zwei exklusive Eingabemodi:

1. ASSET_CLASS: h nach Assetklasse und optional Währung; pro Asset Override als bewusstes Detail. Zum Beispiel Aktien 0.5, Bonds 1.0. Inländische und gesperrte Assets ausnehmen.
2. CURRENCY_OVERLAY: h_c auf das gesamte hedgefähige Fremdwährungsexposure. Danach g_c=h_c*E_c,hedgefaehig.

Immer nur ein aktiver Eingabemodus. Die andere Darstellung ist abgeleitet/read-only. Wechsel von Assetklasse zu Währung kann aggregierte g exakt erhalten, aber klassenbezogene Verteilungen verlieren. Beim Rückwechsel gibt es keine eindeutige Rekonstruktion: ursprüngliche Klassenkonfiguration aufbewahren und explizit wiederherstellen oder eine als solche bezeichnete proportionale Zuordnung verwenden. Nicht stillschweigend einen anderen Hedge erzeugen.

MVP nur 0–100 % Absicherung vorhandener Exposures, kein freies spekulatives FX-Overlay und kein Overhedging. Fehlende Forwardkomponente -> gezielte Fehlermeldung. Basiswährungswechsel macht eigene Währung zum Inland; frühere Basiswährung kann zum Hedge-Exposure werden.

Zentrale UI-Tabelle: Exposure vor Hedge, hedgefähiger Anteil, Hedgequote, Hedge-Nominal, Exposure nach Hedge, Verfügbarkeit. Für EMEQ Quote n/a. Netto-FX und Assetklassenaufteilung auf Wunsch nebeneinander.

## 8. Portfolio- und Renditeschnittstelle

Gesamt-Exposurevektor a=(w,g), Hedge-Nominale zusätzlich zu den voll investierten Assetgewichten. Sum(w)=1; g nicht zu den Kapitalgewichten addieren.

    r_P = w' u + g' z
    Var_P = a' Sigma_B a
    sigma_P = sqrt(Var_P)

Für ein Assetkovarianzmodell mit gegebenem h:

    Sigma_assets,B,h = A(h) * Sigma_B * A(h)'

Risikobeiträge zunächst auf Komponentenebene; negative Beiträge zulassen. Asset-/Klassenbeiträge bei einem reinen Währungsoverlay benötigen eine explizite Zuordnung des Overlays, z.B. proportional zu hedgefähigen Exposures. Diese Zuordnung nicht als eindeutig darstellen. Für zwei SAAs direkt Delta-Exposure und Tracking Error sqrt((a2-a1)'Sigma(a2-a1)) ausweisen.

Renditeannahmen über eine versionierte Schnittstelle anbinden. Der zuvor erstellte Renditerechner liefert geometrische Fünfjahresszenarien, keine unmittelbar aggregierbaren einperiodigen Mittelwerte. Pflichtfelder:

    expectations_id, as_of, horizon, scenario_id, asset_code,
    model_currency, return_definition, fee_basis,
    local_return_assumption bzw. jährliche Szenariobausteine,
    currency_path_assumptions, forward_carry_assumptions,
    uncertainty_description

Im MVP deterministische Fünfjahrespfade nach expliziter Rebalancingregel berechnen; Ergebnisse als Szenariorenditen bezeichnen. Monatliche Zwischenverteilung ist eine eigene Modellannahme. Erst bei konsistentem stochastischem Modell Verlustwahrscheinlichkeiten/Quantile anbieten. Keine 5J-CAGRs gewichten und als Portfolio-CAGR bezeichnen. Keine historische Forward-Rendite als zukünftige Erwartung übernehmen. Kein automatisches physisches-Gold-Modell für GSGCTOT.

Falls einperiodige arithmetische E[u] und E[z] konsistent vorliegen: E[r_P]=w'E[u]+g'E[z]. Bei Umrechnung lokaler erwarteter einfacher Renditen Multiplikation und Cov(r_local,FX) berücksichtigen oder gemeinsam modellierte Pfade verwenden. Währungsszenarien über einen gemeinsamen USD-Numeraire widerspruchsfrei triangulieren. Keine unabhängig inkompatiblen EUR/USD-/USD/CHF-/EUR/CHF-Prognosen.

Cash optional: aus Basiswährungs-Cash einen Monatsreturn ableiten. EU-MARK-Proxy vor 1999 ausdrücklich zulässig. Kein unbemerkter Ersatz durch 3M; Zinswerte als Prozent p.a. korrekt konvertieren. Bei Excess-Darstellung Cash einmal von jedem Assetreturn u abziehen, NICHT von jeder Hedgekomponente z. So ergibt sich bei Sum(w)=1 genau einmal Cash-Abzug im Portfolio. Total-Risiko bleibt der Standard.

## 9. Geplante reine R-Hilfsfunktionen

Jede Funktion erhält explizite Argumente, liefert strukturierte Ergebnisse/Diagnosen und verändert keinen globalen Zustand. Die folgenden Namen sind eine Zielstruktur, keine Verpflichtung zu unnötig kleinen Einzeilern.

| Modul/Funktion | Eingabe | Ergebnis |
|---|---|---|
| read_input_workbook(path, config) | Rohdatei | Wide-Rohdaten und Metadaten |
| parse_month_headers(headers) | Header | Eindeutige Monatsschlüssel und Fehler |
| normalize_series_table(raw) | Wide/Long | date, Code, value; Quellprovenienz |
| validate_metadata(meta, policy) | Metadaten | Typisierte Metadaten, Fehler, Warnungen |
| validate_observations(data, meta) | Long-Daten | Duplikate, Einheiten, Abdeckung, Innenlücken |
| build_series_registry(data, meta) | Geprüfte Daten | Nach Code/Paar indizierter Datensatz |
| normalize_spot_quotes(registry) | Spotwerte/Metadaten | V_C: USD je C |
| normalize_forward_quotes(registry, spots) | Forwardwerte/Metadaten | G_C: 1M USD je C |
| cross_fx_levels(V, G, base_ccy) | Normalisierte Levels | Spot-/Forwardkreuze und Verfügbarkeit |
| index_levels_to_returns(levels) | TR-Levels | Einfache Monatsreturns ohne NA-Überbrückung |
| translate_asset_returns(returns, crosses, meta) | Returns/FX | Ungesicherte u_B |
| build_forward_pnl(crosses) | F_t-1, S_t, S_t-1 | z_B, Carry-/Spot-Audit |
| build_risk_basis(u, z, universe) | Komponenten | X_B, IDs, Verfügbarkeitsmasken |
| resolve_estimation_sample(basis, settings) | Fenster/Missing-Modus | Tatsächliche Schätzbasis + Audit |
| estimate_covariance(sample, method) | Daten/Settings | Monatliche Sigma + Target/Parameter |
| reconstruct_factor_moments(data, factors, settings) | Kurze Historien | Strukturierte gemeinsame Momente + Unsicherheit |
| diagnose_covariance(model) | Sigma/IDs | PSD, Eigenwerte, Kondition, Vol/Korr |
| regularize_covariance(model, policy) | Matrix/Policy | Dokumentierte Reparatur oder Fehler |
| derive_prehedge_exposures(weights, meta, base) | Gewichte/Metadaten | E vor Hedge, Eligibility |
| resolve_hedge_notionals(exposures, hedge_config) | Aktiver Hedge-Modus | g, effektive Quoten, Restexposures |
| build_asset_transform(meta, hedge_config, basis) | Asset/Hedge-Mapping | A(h) und Komponentenzuordnung |
| evaluate_portfolio(weights, g, model) | a und Sigma | Risiko, Beiträge, Diagnosen |
| compare_portfolios(configs, model) | Mehrere a | Differenzen und Tracking Errors |
| build_historical_portfolio_path(u, z, config) | Historie/Regeln | Realisierter Modellpfad nach Kosten |
| validate_expectations(payload, context) | Renditeannahmen | Konsistente Szenario-/Einheitenbeschreibung |
| evaluate_return_scenarios(expectations, config) | Renditebausteine/Regeln | Jahrespfade und Portfolio-Szenario-CAGR |
| build_cash_returns(rates, conventions) | Cashreihen | Monatlicher Cash + Proxy-Audit |
| evaluate_walk_forward(data, specifications) | Trainings-/Testfenster | Zeitlich saubere Risikovalidierung |
| bootstrap_risk_uncertainty(data, config, seed) | Gemeinsame Monatsblöcke | Bedingte Unsicherheitsbandbreiten |
| export_analysis_bundle(result, manifest) | Vollständiger Stand | XLSX/CSV/JSON/RDS nach Bedarf |

Einheitliche Ergebnisklassen für Dataset, RiskBasis, CovarianceModel, PortfolioConfig, ExposureReport, Expectations und AnalysisResult. Fehlercodes stabil halten, fachliche Fehlermeldungen von technischen Logs trennen.

## 10. Shiny-Kommunikation und Zustandsmodell

Nachrichtenbefehle: app.bootstrap, dataset.load, dataset.validate, risk.estimate, portfolio.evaluate, scenarios.evaluate, validation.run, export.prepare. Nicht jede UI-Aktion benötigt einen Serverbefehl.

Jeder Request enthält protocol_version, request_id, type, context und payload. Antworten enthalten dieselbe request_id, Kontext-IDs, state_version, status, result sowie strukturierte warnings/errors. Ergebnisse müssen JSON-tauglich sein: Datum ISO-Format, Zahlen endlich, fehlende Werte null plus Verfügbarkeitsinformation, keine NaN/Infinity. Keine Rundung auf Anzeigepräzision vor Rechnungen. Matrizen mit expliziter Dimension und component_ids serialisieren; JS-Zeilen-/Spaltenorientierung testen.

Im Frontend ein kleiner zentraler Store:

    dataset, metadata, universe, base_currency, risk_settings,
    covariance_models, portfolios, hedge_mode, hedge_configs,
    expectations, results, jobs, connection, dirty_flags

Draft-Eingaben von angewendeter Berechnungskonfiguration trennen. Ergebnisse bleiben bei ungültiger neuer Eingabe sichtbar, aber als vorheriger Stand markiert. Neuere Basiswährungs-/Datensatzwechsel dürfen nicht durch ältere Jobs überschrieben werden. Jobergebnisse nur übernehmen, wenn Request und Kontext noch aktuell sind. Physische Prozessbeendigung ist keine zugesicherte ExtendedTask-Funktion: im MVP Ergebnis verwerfen/Job als überholt markieren und doppelte Aufträge verhindern.

Keine manuell gebaute zweite WebSocket-Verbindung. Kein Full-Page-renderUI bei Slideränderungen. Sicherer Textaufbau mit textContent statt unkontrolliertem innerHTML aus Dateiinhalten. Validierung im Browser dient UX; R validiert jeden fachlichen Request erneut.

## 11. Performance, Cache und Hintergrundjobs

### Cache-Abhängigkeiten

| Ebene | Cache-Schlüssel | Nicht enthalten |
|---|---|---|
| Import | Datei-Inhaltshash, Importversion | Gewichte/Hedgequotes |
| FX/Returns | Datenhash, relevante Metadaten, Basiswährung, Konventionenversion | Gewichte/Hedgequotes/Renditeszenarien |
| Risikobasis/Matrix | Transformhash, Universum, Komponenten, Fenster, Schätzer, Missing-Modell, Hyperparameter, Engineversion | Portfolio-Gewichte und Hedgequotes |
| Portfolioauswertung | Matrix-ID, Gewichte, Hedge-Konfiguration, Kostenkonvention | Tabellenfilter/Navigationszustand |
| Renditeszenarien | Erwartungs-ID, Basiswährung, Gewichte, Hedge-/Rebalancingkonfiguration | Reine Heatmapdarstellung |

Vollständige Datei-/Metadatenhashes einmal beim Einlesen bilden, nicht bei jedem Slider-Event. Nur letztes Datum als Cache-Key reicht nicht, weil historische Werte revidiert sein können. `cachem` bzw. bindCache verwenden; bindCache-Schlüssel steuern auch reaktive Abhängigkeiten und müssen alle Ergebnisinputs abdecken. Aus privaten Uploads keine versehentlich sitzungsübergreifend erreichbaren Ergebnisse machen. Sessioncache als sichere Standardwahl; bewusst gemeinsamer unveränderlicher Hausdatensatz darf pro Prozess geteilt werden.

Matrixberechnungen nur bei relevantem Settingswechsel bzw. explizitem Schätzen-Button. Gewichte/Hedgequoten ändern nur a, A(h) und Ergebnisprojektion. Modelle bei Basiswährungswechsel aus Cache laden, sonst berechnen. Optional CHF/EUR/USD nach erstem Load gezielt vorwärmen; keine unkontrollierte Joblawine.

Rechenkern: Matrixoperationen mit BLAS, Typprüfung/Sortierung einmal, keine wiederholten Excel-Reads, keine rowwise-Schleifen über Daten bei jedem Request. Kleine N×N-Projektionen sind schnell; Parallelität dafür nicht einsetzen.

Lange Jobs (Bootstrap, EM/Faktormodellvalidierung, umfangreiche Imports) über ExtendedTask + tatsächlich asynchrone Promise/Future-Ausführung. Reaktive Inputs vor Jobstart als unveränderliche Argumente snapshotten; Worker liest kein input/session/reactiveVal. Fehler und Ergebnisdaten an Hauptprozess zurückgeben, dort senden/cachen. Future-Plan beim Appstart setzen, nicht pro Request/Session ändern. Workerzahl global begrenzen, für beschränkte Umgebungen zunächst 1–2 und keine verschachtelte Parallelität/BLAS-Überbelegung. Startdiagnostik prüft, ob Hintergrundprozesse in der konkreten Citrix-/Serverumgebung erlaubt sind. Bei fehlender Unterstützung schwere Jobs deaktivieren oder in einen expliziten Batchmodus verlagern; keine scheinbar asynchrone blockierende Funktion.

Slider: sofortige JS-Vorschau aus autoritativer Matrix möglich, danach entprellte R-Prüfung (z.B. 150–250 ms). Keine Roundtrips für Hover, Formatierung oder Auswahl einer bereits vorhandenen Ansicht. Ausgewählte Chart-/Tabellendaten statt vollständiger Rohhistorie übertragen. Kleine gemeinsame Risikomatrix ist browserseitig ausreichend; aufwendige Verteilungsschätzung bleibt in R.

Messbare Zielwerte als zu prüfende Budgets, keine unbelegten Versprechen: 30 Assets, bis 10 Hedgekomponenten, 600 Monate; Browserinteraktion unmittelbar, Warm-Portfolioauswertung einschliesslich Nachrichten idealerweise unter 300 ms, keine wiederholten Full-Page-Neuaufbauten. Kaltimport, Matrixschätzung, Payloadgrösse und Mehrbenutzerverhalten separat messen und Hardware angeben. Schwere Jobs dürfen UI/andere Sitzungen nicht blockieren.

## 12. UI-Arbeitsbereiche

1. Portfolio: Auswahl/Editor mehrerer SAAs, Kapitalgewichte, Summe/Profilquote.
2. Währungen: aktive Basiswährung, Hedge-Modus, Exposuretabelle vor/nach Hedge, nicht hedgefähige EMEQ-Position.
3. Risiko: Modellwahl, Volatilitäten, Korrelationsheatmap, Beiträge und Matrixunterschiede.
4. Rendite: fünfjährige Baustein-/Szenarioannahmen oder deren Import, lokale vs Basiswährungsdarstellung.
5. Vergleich: dieselben SAAs über Risikomatrizen/Hedgevarianten, Tracking Error und Stressunterschiede; keine künstlich präzise Rangliste.
6. Daten & Methodik: Import, Coverage, Metadatenprüfung, Faktor-/Forward-Audit, Validierung, Exporte.

Oben immer kompakt: Portfolio, Basiswährung, aktives Risikomodell, Renditeszenario, Ergebnisstichtag und Aktualitätsstatus. Beim Wechsel keine Eingaben verlieren. Szenario- und Portfolioannahmen können als versionierte JSON-Konfiguration gespeichert/geladen werden. Persistenzstandard: Konfigurationsdateien und Sitzung, kein ungefragtes Hochladen von Hausdaten oder Cloudservice.

## 13. Projektstruktur

```text
app.R
R/
  app_config.R
  server_commands.R
  session_state.R
  cache_jobs.R
  data_import.R
  metadata.R
  fx_quotes.R
  return_transforms.R
  cash_returns.R
  risk_basis.R
  covariance_estimators.R
  missing_history.R
  risk_diagnostics.R
  exposures.R
  portfolio_metrics.R
  expectations_adapter.R
  scenario_paths.R
  validation.R
  exports.R
www/
  index.html
  css/tokens.css
  css/app.css
  js/app.js
  js/store.js
  js/shiny-bridge.js
  js/components/
  js/charts/
  vendor/
config/
  defaults.json
  metadata-schema.json
data/demo/
tests/testthat/
tests/js/
README.md
```

Abhängigkeiten klein halten: Shiny, jsonlite, data.table oder vergleichbares effizientes Tabellenpaket, readxl, digest, cachem, geeignete Shrinkagebibliothek, promises/future und XLSX-Exportbibliothek. Nicht mehrere Pakete mit derselben Funktion ohne Grund laden. Frontendlibraries lokal versionieren, keine zwingende CDN-Verbindung. R-Versionen/Lockfile an tatsächlicher Zielumgebung ausrichten.

## 14. Tests und Abnahme

### Fachliche Unit-Tests

- TR-Level -> Monatsreturn, erster Monat, unterschiedliche Anfänge, Innenlücke nicht überbrückt.
- Quote-Skalierung/Inversion, Outright und Points; GBP-Korrektur; Spot- und Forwarddreiecke.
- CHF/EUR/USD-Wechsel mit USD-Identität und Inlandsanlage.
- Forward t-1 statt t, Null-FX und bekannte Carry-PnLs, voller/halber Hedge, verbleibender Kreuzterm.
- EM in USD korrekt umgerechnet, aber nie in USD-Hedgenenner aufgenommen.
- Futures-Gold nicht automatisch als physisches Gold behandelt.
- Fehlende JPY-Forwards blockieren nur betroffene Hedges; keine stillen Ersatzdaten.
- EUR-Cash-Proxy vor 1999 und Rate/100/Day Count korrekt; Excess-Cash nur einmal auf Portfolioebene.
- Assetklassen- und Währungsmodus ergeben bei identischem g identische Gesamtrendite und Gesamtvarianz.
- Sum(w)=1; g erhöht Sum(w) nicht. Quote bei Exposure 0 n/a. Negative Risikobeiträge erhalten.
- Varianz aus X und a entspricht direkter historischer Portfoliovarianz bei identischem Sample/zentrierter Stichprobenschätzung.
- Sigma_h=A Sigma A' entspricht direkter Asset-Hedge-Stichprobenmatrix vor eigenständigem Shrinkage. Keine Gleichheit separat geschrumpfter Matrizen erzwingen.
- PSD, Reihenfolgenwechsel, beinahe identische Reihen, Nullvarianz, Faktorresidualrisiko.
- Rebalancing-Pfade, kumulierte/geometrische Returns und Kosten ohne Doppelzählung.

### Integrations- und Performance-Tests

- Browser-Shiny-Handshake, wiederholte identische Commands und JSON-Roundtrip.
- Alte Request-Ergebnisse überschreiben keinen neueren Zustand.
- Gewichts-/Hedgeänderung startet KEINE neue Kovarianzschätzung; mit Aufrufzähler testen.
- Daten-/Metadatenrevision invalidiert relevante Caches auch bei gleichem Enddatum.
- Zwei Sitzungen beeinflussen ihre privaten Konfigurationen nicht.
- Hintergrundjob blockiert nicht leichte Interaktionen. Kein Jobs-Queue-Wachstum durch Slider.
- Export ist exakt dem angewendeten, sichtbaren Konfigurationshash zugeordnet.
- UI auf typischem Desktop und kleinerem Bildschirm prüfen: Tabellen, Navigation, Tastatur, Fehlermeldungen, Reconnect.

## 15. Reihenfolge der Implementierung

Phase 1: Datenvertrag/Metadatenmigration, Golden Test Cases und reine R-FX-/Return-Engine. Kleine Demo mit bekannten analytischen Ergebnissen, noch keine umfangreiche UI.

Phase 2: Gemeinsame Risikobasis je Währung, Complete-Case-Shrinkage/EWMA, Diagnostik und Cache. Fehlende Forwards erklärbar behandeln.

Phase 3: Exposure-/Hedge-Engine, zwei exklusive Eingabemodi, feste Portfoliovergleiche und analytische Gleichheitstests.

Phase 4: Individuelle HTML/JS-Oberfläche und Shiny-Bridge. Zunächst vollständig funktionierender vertikaler Durchstich: Datei -> Basiswährung -> Matrix -> Gewichte/Hedge -> Kennzahlen.

Phase 5: Renditerechner-Schnittstelle und deterministische Fünfjahresszenarien, explizite Währungs-/Rebalancingannahmen. Keine unfundierten Verlustwahrscheinlichkeiten.

Phase 6: Kurze-Historien-Faktormodell, Walk-forward, Bootstrap und Stresssichten in Hintergrundjobs.

Phase 7: UX-/Performanceprüfung, unabhängige Rechenkontrolle, Export, Dokumentation und Zielsystem-Starttest.

Liefere nach jeder Phase lauffähigen Code und die zugehörigen bestandenen Tests. Annahmen und noch nicht implementierte Funktionen sichtbar benennen. Kein Design-Mockup als fertig funktionierende Anwendung ausgeben. Keine neue Hostingplattform einführen; Start und späteres Deployment erfolgen in der vorgesehenen R-Shiny-Umgebung. Ein reines HTML/JS-Frontend macht den laufenden Shiny-Prozess und dessen Zugriffs-/Lizenzbedingungen nicht überflüssig.

## 16. Offizielle technische Referenzen

- Individuelle HTML-Templates ohne automatische Bootstrap-UI: https://shiny.posit.co/r/articles/build/templates/
- JS/R-Kommunikation: https://shiny.posit.co/r/articles/build/communicating-with-js/
- Nichtblockierende ExtendedTask-Ausführung: https://shiny.posit.co/r/articles/improve/nonblocking
- Cache-Abhängigkeiten: https://shiny.posit.co/r/reference/shiny/latest/bindCache.html

Bei Implementierung die tatsächlich verwendeten Paketversionen und offiziellen APIs prüfen. Die Architektur verlangt keine bslib-Taskbuttons; Jobzustände werden in den eigenen HTML-Komponenten angezeigt.
