# BEA PCE component crosswalk used by the Employ America trimmed-mean work.
#
# Baseline component list: the current 177-component Dallas workbook at
#   https://www.dallasfed.org/-/media/documents/research/pce/detail
# The Dallas technical note at
#   https://www.dallasfed.org/-/media/documents/research/pce/tech.pdf
# lists 178 post-2009 components in BEA underlying-detail order. It is used for
# BEA ordering and group boundaries only; BEA has since consolidated one line,
# so the current workbook is 177. Do not use 178 as the current count.
# BEA line numbers and descriptions: U20404 (price indexes) and U20405
# (current dollars), underlying detail, accessed through
#   https://apps.bea.gov/iTable/?reqid=19&step=2&isuri=1&categories=underlying
# Other-than-market flags: BEA FAQ 83 composition workbooks,
#   https://apps.bea.gov/faq/national/nonMktBasedComposition.xls
#   https://apps.bea.gov/faq/national/MktBasedComposition.xls
#
# The exact 193-component Employ America crosswalk is unpublished. This is an
# independent 192-component reconstruction from the official sources above. It
# starts from the Dallas 177, splits communication into its six BEA
# constituents, merges new domestic and foreign autos (shared price, summed
# nominal), and splits used autos into a market component (lines 14+16: net
# transactions net of employee reimbursement, with line 14 as the price line)
# plus a separate other-than-market margin component (line 15). This keeps BEA
# parent line 13 exactly equal to 14 + 15 + 16 while preserving the market-
# based net-transactions series. The 14+16 nominal is nonnegative in the
# current panel; BEA's 1962-10 value is the one historical exception
# (-60 million). Other parent rows are split only where BEA
# publishes monthly children and the split is accounting-exact; BEA's official
# other-than-market mask is then applied. `housing` flags the tenant- and
# owner-occupied blocks that correspond to CPI rent of primary residence and
# OER; farm dwellings and group housing are not flagged. Lines 145 and 333 are
# published BEA lines but are excluded from the panel as netting and foreign-
# travel adjustments; lines 157 and 158 are unpublished monthly children of
# tenant-occupied parent 156 and are represented by that parent. Line numbers
# refer to the current U20404/U20405 vintage and must be rechecked when BEA
# renumbers the underlying detail.

.pce_map_text <- r"---(line_number|price_line|component_id|description|housing|market_based|source_group|dallas_name
130|130|pce_l130|Film and photographic supplies|False|True|Nondurable goods|Film and photographic supplies
42|42|pce_l042|Other video equipment|False|True|Durable goods|Other video equipment
22|22|pce_l022|Accessories and parts|False|True|Durable goods|Accessories and parts
71|71|pce_l071|Telephone and related communication equipment|False|True|Durable goods|Telephone and related communication equipment
51|51|pce_l051|Calculators, typewriters, and other information processing equipment|False|True|Durable goods|Calculators, typewriters, and other information processing equipment
127|127|pce_l127|Games, toys, and hobbies|False|True|Nondurable goods|Games, toys, and hobbies
329|329|pce_l329|Moving, storage, and freight services|False|True|Services|Moving, storage, and freight services
50|50|pce_l050|Computer software and accessories|False|True|Durable goods|Computer software and accessories
129|129|pce_l129|Flowers, seeds, and potted plants|False|True|Nondurable goods|Flowers, seeds, and potted plants
64|64|pce_l064|Jewelry|False|True|Durable goods|Jewelry
34|34|pce_l034|Nonelectric cookware and tableware|False|True|Durable goods|Nonelectric cookware and tableware
43|43|pce_l043|Audio equipment|False|True|Durable goods|Audio equipment
86|86|pce_l086|Fresh milk|False|True|Nondurable goods|Fresh milk
171|171|pce_l171|Natural gas|False|True|Services|Natural gas
124|124|pce_l124|Nonprescription drugs|False|True|Nondurable goods|Nonprescription drugs
80|80|pce_l080|Beef and veal|False|True|Nondurable goods|Beef and veal
33|33|pce_l033|Dishes and flatware|False|True|Durable goods|Dishes and flatware
25|25|pce_l025|Furniture|False|True|Durable goods|Furniture
92|92|pce_l092|Vegetables (fresh)|False|True|Nondurable goods|Vegetables (fresh)
342|342|pce_l342|Final consumption expenditures of nonprofit institutions serving households (NPISHs)|False|False|NPISH|Final consumption expenditures of nonprofit institutions serving households
108|108|pce_l108|Children's and infants' clothing|False|True|Nondurable goods|Children's and infants' clothing
69|69|pce_l069|Educational books|False|True|Durable goods|Educational books
94|94|pce_l094|Sugar and sweets|False|True|Nondurable goods|Sugar and sweets
175|175|pce_l175|Dental services|False|True|Services|Dental services
132|132|pce_l132|Household cleaning products|False|True|Nondurable goods|Household cleaning products
214|214|pce_l214|Motion picture theaters|False|True|Services|Motion picture theaters
215|215|pce_l215|Live entertainment, excluding sports|False|True|Services|Live entertainment, excluding sports
217|217|pce_l217|Museums and libraries|False|True|Services|Museums and libraries
324|324|pce_l324|Social advocacy and civic and social organizations|False|False|Services|Social advocacy and civic and social organizations
328|328|pce_l328|Domestic services|False|True|Services|Domestic services
198|198|pce_l198|Parking fees and tolls|False|True|Services|Parking fees and tolls
91|91|pce_l091|Fruit (fresh)|False|True|Nondurable goods|Fruit (fresh)
332|332|pce_l332|Other household services|False|True|Services|Other household services
218|218|pce_l218|Audio-video, photographic, and information processing equipment services|False|True|Services|Audio-video, photographic, and information processing equipment services
256|256|pce_l256|Other depository institutions and regulated investment companies|False|False|Services|Other depository institutions and regulated investment companies
271|271|pce_l271|Life insurance|False|False|Services|Life insurance
36|36|pce_l036|Tools, hardware, and supplies|False|True|Durable goods|Tools, hardware, and supplies
232|232|pce_l232|Package tours|False|True|Services|Package tours
212|212|pce_l212|Amusement parks, campgrounds, and related recreational services|False|True|Services|Amusement parks, campgrounds, and related recreational services
257|257|pce_l257|Pension funds|False|False|Services|Pension funds
77|77|pce_l077|Cereals|False|True|Nondurable goods|Cereals
303|303|pce_l303|Other personal business services|False|True|Services|Other personal business services
112|112|pce_l112|Shoes and other footwear|False|True|Nondurable goods|Shoes and other footwear
100|100|pce_l100|Spirits|False|True|Nondurable goods|Spirits
128|128|pce_l128|Pets and related products|False|True|Nondurable goods|Pets and related products
134|134|pce_l134|Household linens|False|True|Nondurable goods|Household linens
206|206|pce_l206|Other road transportation service|False|True|Services|Other road transportation service
201|201|pce_l201|Railway transportation|False|True|Services|Railway transportation
208|208|pce_l208|Water transportation|False|True|Services|Water transportation
203|203|pce_l203|Intercity buses|False|True|Services|Intercity buses
170|170|pce_l170|Electricity|False|True|Services|Electricity
216|216|pce_l216|Spectator sports|False|True|Services|Spectator sports
95|95|pce_l095|Food products, not elsewhere classified|False|True|Nondurable goods|Food products, not elsewhere classified
304|304|pce_l304|Labor organization dues|False|False|Services|Labor organization dues
194|194|pce_l194|Motor vehicle leasing|False|True|Services|Motor vehicle leasing
211|211|pce_l211|Membership clubs and participant sports centers|False|True|Services|Membership clubs and participant sports centers
45|45|pce_l045|Audio discs, tapes, vinyl, and permanent digital downloads|False|True|Durable goods|Audio discs, tapes, vinyl, and permanent digital downloads
123|123|pce_l123|Prescription drugs|False|True|Nondurable goods|Prescription drugs
101|101|pce_l101|Wine|False|True|Nondurable goods|Wine
52|52|pce_l052|Sporting equipment, supplies, guns, and ammunition|False|True|Durable goods|Sporting equipment, supplies, guns, and ammunition
54|54|pce_l054|Motorcycles|False|True|Durable goods|Motorcycles
103|103|pce_l103|Food produced and consumed on farms|False|False|Nondurable goods|Food produced and consumed on farms
317|317|pce_l317|Social assistance|False|False|Services|Social assistance
21|21|pce_l021|Tires|False|True|Durable goods|Tires
279|279|pce_l279|Net motor vehicle and other transportation insurance|False|False|Services|Net motor vehicle and other transportation insurance
84|84|pce_l084|Fish and seafood|False|True|Nondurable goods|Fish and seafood
97|97|pce_l097|Coffee, tea, and other beverage materials|False|True|Nondurable goods|Coffee, tea, and other beverage materials
272|272|pce_l272|Net household insurance|False|True|Services|Net household insurance
177|177|pce_l177|Home health care|False|True|Services|Paramedical services
178|178|pce_l178|Medical laboratories|False|True|Services|Paramedical services
180|180|pce_l180|Specialty outpatient care facilities and health and allied services|False|True|Services|Paramedical services
181|181|pce_l181|All other professional medical services|False|True|Services|Paramedical services
174|174|pce_l174|Physician services|False|True|Services|Physician services
135|135|pce_l135|Sewing items|False|True|Nondurable goods|Sewing items
110|110|pce_l110|Clothing materials|False|True|Nondurable goods|Clothing materials
326|326|pce_l326|Foundations and grantmaking and giving services to households|False|False|Services|Foundations and grantmaking and giving services to households
245|245|pce_l245|Alcohol in purchased meals|False|True|Services|Alcohol in purchased meals
78|78|pce_l078|Bakery products|False|True|Nondurable goods|Bakery products
188|188|pce_l188|Nonprofit nursing homes' services to households|False|True|Services|Nursing homes
189|189|pce_l189|Proprietary and government nursing homes|False|True|Services|Nursing homes
302|302|pce_l302|Employment agency services|False|True|Services|Employment agency services
68|68|pce_l068|Corrective eyeglasses and contact lenses|False|True|Durable goods|Corrective eyeglasses and contact lenses
138|138|pce_l138|Hair, dental, shaving, and miscellaneous personal care products except electrical products|False|True|Nondurable goods|Hair, dental, shaving, and miscellaneous personal care products except electrical products
140|140|pce_l140|Electric appliances for personal care|False|True|Nondurable goods|Electric appliances for personal care
164|164|pce_l164|Group housing|False|True|Services|Group housing
156|156|pce_l156|Tenant-occupied stationary homes and landlord durables|True|True|Services|Tenant-occupied stationary homes and landlord durables
155|155|pce_l155|Tenant-occupied mobile homes|True|True|Services|Tenant-occupied mobile homes
139|139|pce_l139|Cosmetic / perfumes / bath / nail preparations and implements|False|True|Nondurable goods|Cosmetic / perfumes / bath / nail preparations and implements
161|161|pce_l161|Owner-occupied mobile homes|True|True|Services|Owner-occupied mobile homes
163|163|pce_l163|Rental value of farm dwellings|False|False|Services|Rental value of farm dwellings
162|162|pce_l162|Owner-occupied stationary homes|True|True|Services|Owner-occupied stationary homes
111|111|pce_l111|Standard clothing issued to military personnel|False|False|Nondurable goods|Standard clothing issued to military personnel
312|312|pce_l312|Laundry and drycleaning services|False|True|Services|Laundry and drycleaning services
275|275|pce_l275|Net health insurance|False|False|Services|Net health insurance
241|241|pce_l241|Other purchased meals|False|True|Services|Other purchased meals
106|106|pce_l106|Women's and girls' clothing|False|True|Nondurable goods|Women's and girls' clothing
98|98|pce_l098|Mineral waters, soft drinks, and vegetable juices|False|True|Nondurable goods|Mineral waters, soft drinks, and vegetable juices
306|306|pce_l306|Funeral and burial services|False|True|Services|Funeral and burial services
102|102|pce_l102|Beer|False|True|Nondurable goods|Beer
325|325|pce_l325|Religious organizations' services to households|False|False|Services|Religious organizations' services to households
167|167|pce_l167|Water supply and sewage maintenance|False|True|Services|Water supply and sewage maintenance
255|255|pce_l255|Commercial banks|False|False|Services|Commercial banks
28|28|pce_l028|Window coverings|False|True|Durable goods|Window coverings
9|9|pce_l009|New light trucks|False|True|Durable goods|New light trucks
7+8|7|pce_l007_l008|New domestic and foreign autos (shared price)|False|True|Durable goods|New domestic autos + New foreign autos
14+16|14|pce_l014_l016|Net transactions in used autos, net of employee reimbursement|False|True|Durable goods|Used autos
15|15|pce_l015|Used auto margin|False|False|Durable goods|Used autos
61|61|pce_l061|Musical instruments|False|True|Durable goods|Musical instruments
81|81|pce_l081|Pork|False|True|Nondurable goods|Pork
83|83|pce_l083|Poultry|False|True|Nondurable goods|Poultry
18|18|pce_l018|Net transactions in used trucks|False|True|Durable goods|Used light trucks
19|19|pce_l019|Used truck margin|False|False|Durable goods|Used light trucks
136|136|pce_l136|Miscellaneous household products|False|True|Nondurable goods|Miscellaneous household products
309|309|pce_l309|Hairdressing salons and personal grooming establishments|False|True|Services|Hairdressing salons and personal grooming establishments
227|227|pce_l227|Casino gambling|False|False|Services|Casino gambling
228|228|pce_l228|Lotteries|False|False|Services|Lotteries
229|229|pce_l229|Pari-mutuel net receipts|False|False|Services|Pari-mutuel net receipts
247|247|pce_l247|Food supplied to civilians|False|False|Services|Food supplied to civilians
248|248|pce_l248|Food supplied to military|False|False|Services|Food supplied to military
240|240|pce_l240|Higher education school lunches|False|True|Services|Higher education school lunches
292|292|pce_l292|Proprietary and public higher education|False|True|Services|Proprietary and public higher education
293|293|pce_l293|Nonprofit private higher education services to households|False|True|Services|Nonprofit private higher education services to households
185|185|pce_l185|Proprietary hospitals|False|True|Services|Proprietary hospitals
184|184|pce_l184|Nonprofit hospitals' services to households|False|True|Services|Nonprofit hospitals' services to households
186|186|pce_l186|Government hospitals|False|True|Services|Government hospitals
93|93|pce_l093|Processed fruits and vegetables|False|True|Nondurable goods|Processed fruits and vegetables
107|107|pce_l107|Men's and boys' clothing|False|True|Nondurable goods|Men's and boys' clothing
133|133|pce_l133|Household paper products|False|True|Nondurable goods|Household paper products
310|310|pce_l310|Miscellaneous personal care services|False|True|Services|Miscellaneous personal care services
314|314|pce_l314|Repair and hire of footwear|False|True|Services|Repair and hire of footwear
313|313|pce_l313|Clothing repair, rental, and alterations|False|True|Services|Clothing repair, rental, and alterations
239|239|pce_l239|Elementary and secondary school lunches|False|True|Services|Elementary and secondary school lunches
330|330|pce_l330|Repair of furniture, furnishings, and floor coverings|False|True|Services|Repair of furniture, furnishings, and floor coverings
331|331|pce_l331|Repair of household appliances|False|True|Services|Repair of household appliances
141|141|pce_l141|Tobacco|False|True|Nondurable goods|Tobacco
251|251|pce_l251|Housing at schools|False|True|Services|Housing at schools
297|297|pce_l297|Commercial and vocational schools|False|True|Services|Commercial and vocational schools
65|65|pce_l065|Watches|False|True|Durable goods|Watches
259|259|pce_l259|Financial service charges and fees|False|True|Services|Financial service charges, fees, and commissions
261|261|pce_l261|Direct commissions|False|True|Services|Financial service charges, fees, and commissions
264|264|pce_l264|Indirect commissions|False|True|Services|Financial service charges, fees, and commissions
267|267|pce_l267|Mutual fund sales charges|False|False|Services|Financial service charges, fees, and commissions
268|268|pce_l268|Portfolio management and investment advice services|False|False|Services|Financial service charges, fees, and commissions
269|269|pce_l269|Trust, fiduciary, and custody activities|False|False|Services|Financial service charges, fees, and commissions
231|231|pce_l231|Veterinary and other services for pets|False|True|Services|Veterinary and other services for pets
204|204|pce_l204|Taxicabs and ride sharing services|False|True|Services|Taxicabs and ride sharing services
205|205|pce_l205|Intracity mass transit|False|True|Services|Intracity mass transit
119|119|pce_l119|Other fuels|False|True|Nondurable goods|Other fuels
316|316|pce_l316|Child care|False|True|Services|Child care
296|296|pce_l296|Day care and nursery schools|False|True|Services|Day care and nursery schools
233|233|pce_l233|Maintenance and repair of recreational vehicles and sports equipment|False|True|Services|Maintenance and repair of recreational vehicles and sports equipment
37|37|pce_l037|Outdoor equipment and supplies|False|True|Durable goods|Outdoor equipment and supplies
125|125|pce_l125|Other medical products|False|True|Nondurable goods|Other medical products
67|67|pce_l067|Therapeutic medical equipment|False|True|Durable goods|Therapeutic medical equipment
168|168|pce_l168|Garbage and trash collection|False|True|Services|Garbage and trash collection
192|192|pce_l192|Motor vehicle maintenance and repair|False|True|Services|Motor vehicle maintenance and repair
26|26|pce_l026|Clocks, lamps, lighting fixtures, and other household decorative items|False|True|Durable goods|Clocks, lamps, lighting fixtures, and other household decorative items
87|87|pce_l087|Processed dairy products|False|True|Nondurable goods|Processed dairy products
301|301|pce_l301|Tax preparation and other related services|False|True|Services|Tax preparation and other related services
82|82|pce_l082|Other meats|False|True|Nondurable goods|Other meats
70|70|pce_l070|Luggage and similar personal items|False|True|Durable goods|Luggage and similar personal items
31|31|pce_l031|Small electric household appliances|False|True|Durable goods|Small electric household appliances
47|47|pce_l047|Photographic equipment|False|True|Durable goods|Photographic equipment
41|41|pce_l041|Televisions|False|True|Durable goods|Televisions
295|295|pce_l295|Elementary and secondary schools|False|True|Services|Elementary and secondary schools
30|30|pce_l030|Major household appliances|False|True|Durable goods|Major household appliances
27|27|pce_l027|Carpets and other floor coverings|False|True|Durable goods|Carpets and other floor coverings
89|89|pce_l089|Fats and oils|False|True|Nondurable goods|Fats and oils
144|144|pce_l144|Stationery and miscellaneous printed materials|False|True|Nondurable goods|Stationery and miscellaneous printed materials
59|59|pce_l059|Other recreational vehicles|False|True|Durable goods|Other recreational vehicles
58|58|pce_l058|Pleasure aircraft|False|True|Durable goods|Pleasure aircraft
57|57|pce_l057|Pleasure boats|False|True|Durable goods|Pleasure boats
55|55|pce_l055|Bicycles and accessories|False|True|Durable goods|Bicycles and accessories
305|305|pce_l305|Professional association dues|False|True|Services|Professional association dues
299|299|pce_l299|Legal services|False|True|Services|Legal services
283|283|pce_l283|Land-line telephone services, local charges|False|True|Services|Communication
284|284|pce_l284|Land-line telephone services, long-distance charges|False|True|Services|Communication
285|285|pce_l285|Cellular telephone services|False|True|Services|Communication
287|287|pce_l287|First-class postal service (by U.S. Postal Service)|False|True|Services|Communication
288|288|pce_l288|Other delivery services (by non-U.S. postal facilities)|False|True|Services|Communication
289|289|pce_l289|Internet access|False|True|Services|Communication
197|197|pce_l197|Motor vehicle rental|False|True|Services|Motor vehicle rental
60|60|pce_l060|Recreational books|False|True|Durable goods|Recreational books
116|116|pce_l116|Lubricants and fluids|False|True|Nondurable goods|Lubricants and fluids
250|250|pce_l250|Hotels and motels|False|True|Services|Hotels and motels
88|88|pce_l088|Eggs|False|True|Nondurable goods|Eggs
207|207|pce_l207|Air transportation|False|True|Services|Air transportation
46|46|pce_l046|Video discs, tapes, and permanent digital downloads|False|True|Durable goods|Video discs, tapes, and permanent digital downloads
143|143|pce_l143|Newspapers and periodicals|False|True|Nondurable goods|Newspapers and periodicals
49|49|pce_l049|Personal computers/tablets and peripheral equipment|False|True|Durable goods|Personal computers/tablets and peripheral equipment
115|115|pce_l115|Gasoline and other motor fuel|False|True|Nondurable goods|Gasoline and other motor fuel
118|118|pce_l118|Fuel oil|False|True|Nondurable goods|Fuel oil)---"

.pce_crosswalk <- function() {
  cw <- data.table::fread(
    text = .pce_map_text,
    sep = "|",
    header = TRUE,
    colClasses = c(
      "character", "integer", "character", "character",
      "logical", "logical", "character", "character"
    ),
    data.table = TRUE,
    strip.white = TRUE
  )
  if (nrow(cw) != 192L) {
    stop("Internal PCE crosswalk must contain 192 components.", call. = FALSE)
  }
  if (anyDuplicated(cw$component_id)) {
    stop("Internal PCE crosswalk has duplicated component_id values.", call. = FALSE)
  }
  if (anyNA(cw$price_line)) {
    stop("Internal PCE crosswalk has missing price_line values.", call. = FALSE)
  }
  line_sets <- strsplit(cw$line_number, "+", fixed = TRUE)
  price_in_set <- mapply(
    function(pl, ls) as.character(pl) %in% ls,
    cw$price_line, line_sets
  )
  if (!all(price_in_set)) {
    stop("Internal PCE crosswalk has a price_line outside its component lines.", call. = FALSE)
  }
  cw[]
}

.pce_expand_lines <- function(cw) {
  lm <- cw[, {
    ln <- as.integer(strsplit(line_number, "+", fixed = TRUE)[[1L]])
    list(line_number = ln, line_order = seq_along(ln))
  }, by = .(
    component_id, component_line = line_number, description,
    housing, market_based, source_group, dallas_name
  )]
  if (anyDuplicated(lm$line_number)) {
    stop("Internal PCE crosswalk assigns a BEA line to more than one component.", call. = FALSE)
  }
  lm[]
}

.pce_prepare_input <- function(x, arg) {
  if (!data.table::is.data.table(x)) {
    stop(sprintf("`%s` must be a data.table.", arg), call. = FALSE)
  }
  required <- c("line_number", "series_code", "description", "date", "value")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(sprintf(
      "`%s` is missing required column(s): %s.",
      arg, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  out <- x[, required, with = FALSE]

  line_number_int <- suppressWarnings(as.integer(as.character(out$line_number)))
  if (anyNA(line_number_int)) {
    stop(sprintf("`%s$line_number` must be coercible to integer.", arg), call. = FALSE)
  }
  out[, line_number := line_number_int]

  date_value <- out$date
  if (inherits(date_value, "Date")) {
    date_value <- as.Date(date_value)
  } else if (inherits(date_value, "IDate") || inherits(date_value, "POSIXt")) {
    date_value <- as.Date(date_value)
  } else {
    date_value <- as.Date(
      as.character(date_value),
      tryFormats = c("%Y-%m-%d", "%Y-%m", "%m/%d/%Y", "%Y%m%d")
    )
  }
  if (anyNA(date_value)) {
    stop(sprintf("`%s$date` contains values that cannot be parsed as dates.", arg), call. = FALSE)
  }
  out[, date := date_value]

  value_num <- out$value
  if (!is.numeric(value_num)) {
    value_num <- suppressWarnings(as.numeric(gsub(",", "", as.character(value_num), fixed = TRUE)))
    bad <- is.na(value_num) & !is.na(out$value)
    if (any(bad)) {
      stop(sprintf(
        "`%s$value` contains %d non-numeric value(s).",
        arg, sum(bad)
      ), call. = FALSE)
    }
  }
  out[, value := as.numeric(value_num)]
  out[, series_code := as.character(series_code)]
  out[, description := as.character(description)]

  duplicates <- out[, .N, by = .(line_number, date)][N > 1L]
  if (nrow(duplicates)) {
    stop(sprintf(
      "`%s` has duplicate line_number-date rows (first: line %s on %s).",
      arg, duplicates$line_number[1L], format(duplicates$date[1L])
    ), call. = FALSE)
  }
  out[]
}

# Build the long component panel. `price` and `nominal` must be long BEA
# tables with line_number, series_code, description, date, and value. With
# strict = TRUE every mapped line must be present; strict = FALSE drops
# components with missing lines and allows NA component-date cells.
# `series_code` is the designated price line's code; when the price and nominal
# codes differ, both are returned as "price|nominal". Nominal codes for a
# multi-line component are joined with "+".
prepare_component_panel <- function(price, nominal, strict = TRUE) {
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("`strict` must be a single non-missing logical (TRUE or FALSE).", call. = FALSE)
  }
  cw <- .pce_crosswalk()
  lm <- .pce_expand_lines(cw)
  comps <- unique(cw[, .(
    component_id, line_number, description,
    housing, market_based, source_group
  )])

  p <- .pce_prepare_input(price, "price")
  n <- .pce_prepare_input(nominal, "nominal")

  required_lines <- sort(unique(lm$line_number))
  missing_price <- setdiff(required_lines, unique(p$line_number))
  missing_nominal <- setdiff(required_lines, unique(n$line_number))
  if (length(missing_price) || length(missing_nominal)) {
    message <- paste0(
      "The inputs do not cover every BEA line in the crosswalk.",
      if (length(missing_price)) {
        paste0("\n  Missing from `price`: ", paste(missing_price, collapse = ", "))
      } else "",
      if (length(missing_nominal)) {
        paste0("\n  Missing from `nominal`: ", paste(missing_nominal, collapse = ", "))
      } else ""
    )
    if (strict) {
      stop(message, call. = FALSE)
    }
    warning(message, call. = FALSE)
    shared_lines <- intersect(unique(p$line_number), unique(n$line_number))
    keep <- lm[, .(complete = all(line_number %in% shared_lines)), by = component_id][
      complete == TRUE, component_id
    ]
    lm <- lm[component_id %in% keep]
    comps <- comps[component_id %in% keep]
    if (!nrow(comps)) {
      stop("No crosswalk component has complete price and nominal coverage.", call. = FALSE)
    }
  }

  line_counts <- lm[, .(component_lines = .N), by = component_id]
  price_map <- unique(cw[, .(component_id, price_line)])
  p <- merge(
    p, lm[, .(line_number, component_id, line_order)],
    by = "line_number", all = FALSE, sort = FALSE
  )
  n <- merge(
    n, lm[, .(line_number, component_id, line_order)],
    by = "line_number", all = FALSE, sort = FALSE
  )
  p <- merge(p, price_map, by = "component_id", all.x = TRUE, sort = FALSE)
  n <- merge(n, line_counts, by = "component_id", all.x = TRUE, sort = FALSE)
  p[, description := NULL]
  n[, description := NULL]

  data.table::setorder(p, component_id, date, line_order)
  price_by_component <- p[
    line_number == price_line,
    .(price = value[1L]),
    by = .(component_id, date)
  ]

  nominal_by_component <- n[, .(
    nominal = sum(value),
    nominal_lines = data.table::uniqueN(line_number),
    component_lines = component_lines[1L]
  ), by = .(component_id, date)]
  nominal_by_component[
    nominal_lines < component_lines,
    nominal := NA_real_
  ]
  nominal_by_component[, c("nominal_lines", "component_lines") := NULL]

  dates_price <- sort(unique(p$date))
  dates_nominal <- sort(unique(n$date))
  dates <- intersect(dates_price, dates_nominal)
  if (!length(dates)) {
    stop("`price` and `nominal` have no dates in common.", call. = FALSE)
  }
  if (length(dates) < length(dates_price) || length(dates) < length(dates_nominal)) {
    warning("`price` and `nominal` have different date coverage; using their intersection.", call. = FALSE)
  }

  grid <- data.table::CJ(
    component_id = comps$component_id,
    date = dates,
    unique = TRUE
  )
  out <- merge(grid, price_by_component, by = c("component_id", "date"), all.x = TRUE, sort = FALSE)
  out <- merge(out, nominal_by_component, by = c("component_id", "date"), all.x = TRUE, sort = FALSE)

  incomplete <- out[is.na(price) | is.na(nominal)]
  if (nrow(incomplete)) {
    message <- sprintf(
      "Incomplete component panel: %d component-date cells are missing price or nominal (first: %s on %s).",
      nrow(incomplete), incomplete$component_id[1L], format(incomplete$date[1L])
    )
    if (strict) {
      stop(message, call. = FALSE)
    }
    warning(message, call. = FALSE)
  }

  codes <- data.table::rbindlist(list(
    p[line_number == price_line, .(line_number, series_code, measure = "price")],
    n[, .(line_number, series_code, measure = "nominal")]
  ), use.names = TRUE)
  codes <- unique(codes[!is.na(series_code) & nzchar(series_code)])
  codes <- merge(
    lm[, .(line_number, component_id, line_order)], codes,
    by = "line_number", all.x = TRUE, sort = FALSE
  )
  data.table::setorder(codes, component_id, line_order)
  codes_by_component <- codes[, .(
    series_code = {
      price_codes <- unique(series_code[
        measure == "price" & !is.na(series_code) & nzchar(series_code)
      ])
      nominal_codes <- unique(series_code[
        measure == "nominal" & !is.na(series_code) & nzchar(series_code)
      ])
      price_code <- if (length(price_codes)) paste(price_codes, collapse = "+") else NA_character_
      nominal_code <- if (length(nominal_codes)) paste(nominal_codes, collapse = "+") else NA_character_
      if (!is.na(price_code) && !is.na(nominal_code) && !identical(price_code, nominal_code)) {
        paste(price_code, nominal_code, sep = "|")
      } else if (!is.na(price_code)) {
        price_code
      } else {
        nominal_code
      }
    }
  ), by = component_id]

  out <- merge(out, comps, by = "component_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, codes_by_component, by = "component_id", all.x = TRUE, sort = FALSE)
  data.table::setcolorder(out, c(
    "date", "component_id", "line_number", "series_code", "description",
    "price", "nominal", "housing", "market_based", "source_group"
  ))
  data.table::setorder(out, date, component_id)

  attr(out, "n_components") <- nrow(comps)
  attr(out, "n_bea_lines") <- nrow(lm)
  attr(out, "crosswalk") <- "BEA U20404/U20405; Dallas Fed 177 baseline; 192-component reconstruction; BEA market-based composition"
  attr(out, "excluded_lines") <- c(145L, 333L)
  attr(out, "excluded_line_reasons") <- c(
    "145" = "netting of expenditures abroad",
    "333" = "net foreign travel"
  )
  out[]
}
