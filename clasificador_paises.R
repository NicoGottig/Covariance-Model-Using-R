
# Continente América del Sur
codigos_am_s <- c("AR","BO","BR","CL","CO","EC","FK","GF","GY","PY","PE","SR","UY","VE")
paises_am_s <- countrycode(codigos_am_s, "iso2c", "country.name")

codigos_am_nc <- c("AI","AG","AW","BS","BB","BZ","BM","VG","CA","KY","CR","CU","CW","DM","DO","SV","GL","GD","GP","GT","HT","HN","JM","MQ","MX","MS","NI","PA","PR","BL","KN","LC","MF","PM","VC","SX","TT","TC","US","VI")
paises_am_nc <- countrycode(codigos_am_nc, "iso2c", "country.name")

# Continente Europa
codigos_europa <- c("AL","AD","AT","BY","BE","BA","BG","HR","CY","CZ","DK","EE","FO","FI","FR","DE","GI","GR","HU","IS","IE","IM","IT","XK","LV","LI","LT","LU","MK","MT","MD","MC","ME","NL","NO","PL","PT","RO","RU","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA")
paises_europa <- countrycode(codigos_europa, "iso2c", "country.name", custom_match = list(XK = "Kosovo"))
paises_europa <- unlist(paises_europa)


# Continente África
codigos_africa <- c("DZ","AO","BJ","BW","BF","BI","CM","CV","CF","TD","KM","CD","CG","CI","DJ","EG","GQ","ER","ET","GA","GM","GH","GN","GW","KE","LS","LR","LY","MG","MW","ML","MR","MU","YT","MA","MZ","NA","NE","NG","RW","ST","SN","SC","SL","SO","ZA","SS","SD","SZ","TZ","TG","TN","UG","EH","ZM","ZW")
paises_africa <- countrycode(codigos_africa, "iso2c", "country.name")

# Continente Asia
codigos_asia <- c("AF","AM","AZ","BH","BD","BT","BN","KH","CN","CY","GE","HK","IN","ID","IR","IQ","IL","JP","JO","KZ","KW","KG","LA","LB","MO","MY","MV","MN","MM","NP","KP","OM","PK","PS","PH","QA","SA","SG","KR","LK","SY","TW","TJ","TH","TL","TR","TM","AE","UZ","VN","YE")
paises_asia <- countrycode(codigos_asia, "iso2c", "country.name")

# Continente Oceanía
codigos_oceania <- c("AS","AU","CK","FJ","PF","GU","KI","MH","FM","NR","NC","NZ","NU","MP","PW","PG","PN","WS","SB","TK","TO","TV","UM","VU","WF")
paises_oceania <- countrycode(codigos_oceania, "iso2c", "country.name")

# Continente Antártida
codigos_antartida <- c("AQ")
paises_antartida <- countrycode(codigos_antartida, "iso2c", "country.name")

rm(codigos_africa, codigos_am_s, codigos_am_nc, codigos_antartida, codigos_asia, codigos_europa, codigos_oceania)

## Correccion para modelo lineal (maestr.)
paises_am_nc <- c(paises_am_nc, 
                  "Virgin Islands (U.S.)",
                  "Antigua and Barbuda", 
                  "Bahamas, The",
                  "Sint Maarten (Dutch part)",
                  "St. Kitts and Nevis",
                  "Turks and Caicos Islands")

paises_am_s <- c(paises_am_s,
                 "Curacao",
                 "Trinidad and Tobago",
                 "Venezuela, RB")

paises_europa <- c(paises_europa,
                   "Channel Islands",
                   "Bosnia and Herzegovina",
                   "Slovak Republic",
                   "St. Martin (French part)",
                   "St. Vincent and the Grenadines",
                   "Turkiye"
                   )

paises_asia <- c(paises_asia,
                 "Brunei Darussalam",
                 "Hong Kong SAR, China",
                 "Iran, Islamic Rep.",
                 "Korea, Dem. People's Rep.",
                 "Korea, Rep.",
                 "Kyrgyz Republic",
                 "Lao PDR",
                 "Macao SAR, China",
                 "Myanmar",
                 "Russian Federation",
                 "Syrian Arab Republic",
                 "West Bank and Gaza", 
                 "Yemen, Rep."
                 )

paises_africa <- c(paises_africa,
                   "Congo, Dem. Rep.",
                   "Congo, Rep.",
                   "Egypt, Arab Rep.",
                   "Gambia, The",
                   "Cabo Verde",
                   "Cote d'Ivoire",
                   "Sao Tome and Principe",
                   "Sub-Saharan Africa (excluding high income)")

paises_oceania <- c(paises_oceania, 
                    "Micronesia, Fed. Sts.")

