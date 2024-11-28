
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OESR

<!-- badges: start -->

<!-- badges: end -->

The goal of OESR is to …

## Installation

You can install the development version of OESR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("edgar-treischl/OESR")
```

# Readme

Dies ist die Dokumentation zur Erstellung von Evaluationsberichten. Der
Quellcode beinhaltet auch Funktionen aus dem Paket `limer` zum
Herunterladen von Daten aus Limesurvey. Das Readme File zeigt den Code
der automatisiert PDFs generiert und als Cron Job bei HWS zu
implementieren ist.

Die Datei `run_final.R` beinhaltet alle nötigen Schritte, um
automatisiert PDFs zu generieren.

Um auf die Daten von Limesurvey zugreifen zu können, müssen die
Zugangsdaten, der Server und der Benutzername für Limesurvey festgelegt
werden.

Als nächstes benötigen wir eine Liste mit Angaben über die Schule
(Schulnummer, Schulart, etc.) um einen Bericht zu erstellen. Die
Funktion `get_snr()` gibt einen Datensatz mit den in Limesurvey
aufgelisteten Umfragen zurück, die aktiv und abgelaufen sind. Das
Argument `expired` gibt nur die Umfragen zurück, die gestern abgelaufen
sind; oder, falls alle Umfragen benötigt werden, via `all`.

``` r
#Are any surveys expired yesterday?
snrlist <- get_snr(expired = "yesterday")
snrlist

#> Lucky fellow, there are new expired surveys!
#> # A tibble:
#>   snr   ubb   ganztag audience stype results    
#>   <chr> <lgl> <lgl>   <chr>    <chr> <chr>      
#> 1 0001  FALSE FALSE   leh      gy    Lehrkraefte
```

Der letzte Schritt läuft nur, wenn gestern Umfragen abgelaufen und diese
gelistet werden. Um die Reports für die Umfragen zu generieren, wird aus
dem Datensatz (`snrlist`) eine Liste geniert (`mylist`). Danach nutzt
die Funktion `pwalk()` aus dem `purrr` Package eine Wrapper Funktion
namens `create_reports()`, um alle benötigen Schritte durchzuführen.
Wenn der letzte Schritt erfolgreich durchlaufen wird, sollte ein PDF
Report für jeden gelisteten Fall der `snrlist` generiert werden.

``` r
#Only in case of new reports
if (exists("snrlist") == TRUE) {
  
  #Make a list
  mylist <- list(snr = snrlist$snr,
                 ganztag = snrlist$ganztag,
                 audience = snrlist$audience,
                 ubb = snrlist$ubb,
                 stype = snrlist$stype,
                 results = snrlist$results)
  
  #Create reports based on snr list
  purrr::pwalk(mylist, create_reports)
}
```

Weitere Informationen über die Wrapper Funktionen, Struktur des
Projekts, etc. kann der Vignette entnommen werden.

# To Be Done

Anpassung der Daten für die Filterfragen. Bislang werfen wir alles weg,
was “k. A.” ist.

``` r
data <- data |> dplyr::filter(vals != "k. A.")
```

Dokumentation des Automatisierungssystems. Beispiel: In Zukunft wird ein
neues Template geben, wie greift eine Veränderung der Templates in den
Code ein?

``` r
surveystring <- "0001_202425_ubb_allg_gy_eva_00_2022_p1"
```

- SNR: Identifikation aller Umfragen via ersten vier Zeichen: 0001
- UBB: Identifikation der UBB/Umfrage via String `_ubb_`
- Schulart: Identifikation der Schulart via String `_gy_`, `_rs_`, etc.
- Ganztag: Identifikation der Ganztagsschule via String `_p2` (`p3` und
  `p4` werden im Vorfeld auf `p1` bzw. `p2` zurück gesetzt)
