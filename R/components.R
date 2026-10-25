# BEA PCE component crosswalk used by the Employ America trimmed-mean work.
#
# Public entry points:
#   prepare_component_panel(price, nominal, strict = TRUE)
#   update_bea_line_locks(price, nominal, path = "config", dry_run = FALSE)
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
#
# BEA line and crosswalk integrity locks are tracked in
#   config/bea_line_lock.csv      every line in each current U20404/U20405 table
#   config/crosswalk_lock.csv     the 194 expanded component lines and flags
# prepare_component_panel() compares both locks with the supplied BEA tables and
# stops with a readable diff on any added, removed, renumbered, renamed, or
# recoded line. The locks are never rewritten automatically. After reviewing a
# BEA change, accept it deliberately with
#   update_bea_line_locks(price, nominal)
# and commit the regenerated lock files. Ordinary monthly releases do not change
# line metadata and therefore pass without touching the locks.

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

.pce_expected_components <- 192L
.pce_expected_bea_lines <- 194L
.pce_excluded_lines <- c(145L, 333L)
.pce_excluded_line_reasons <- c(
  "145" = "netting of expenditures abroad",
  "333" = "net foreign travel"
)
.pce_housing_lines <- c(155L, 156L, 161L, 162L)
.pce_expected_market_components <- 167L
.pce_expected_housing_components <- 4L
.pce_source_groups <- c("Durable goods", "Nondurable goods", "Services", "NPISH")
.pce_identity_tolerance <- 5
.pce_coverage_tolerance <- 50
.pce_market_share_range <- c(0.70, 0.95)
.pce_housing_share_range <- c(0.05, 0.30)
# Nominal-dollar accounting identities. Price indexes are not additive, so
# these are checked against U20405 only. The $5 million tolerance reflects
# BEA's million-dollar rounding across the published parent and child lines.
.pce_identity_specs <- list(
  list(
    label = "Used autos (line 13 = 14 + 15 + 16)",
    parent = 13L, children = c(14L, 15L, 16L)
  ),
  list(
    label = "Used light trucks (line 17 = 18 + 19)",
    parent = 17L, children = c(18L, 19L)
  ),
  list(
    label = "Communication (line 281 = 282 + 286 + 289)",
    parent = 281L, children = c(282L, 286L, 289L)
  ),
  list(
    label = "Telecommunication services (line 282 = 283 + 284 + 285)",
    parent = 282L, children = c(283L, 284L, 285L)
  ),
  list(
    label = "Postal and delivery services (line 286 = 287 + 288)",
    parent = 286L, children = c(287L, 288L)
  )
)

.pce_project_root <- function() {
  root <- Sys.getenv("BEA_PROJECT_ROOT", unset = "")
  if (!nzchar(root)) {
    root <- getOption("pce.project_root", "")
    if (is.null(root)) root <- ""
  }
  if (!nzchar(root)) {
    wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    root <- wd
    markers <- c("R/components.R", ".git")
    if (!any(file.exists(file.path(wd, markers)))) {
      cur <- wd
      repeat {
        parent <- dirname(cur)
        if (identical(parent, cur)) break
        cur <- parent
        if (any(file.exists(file.path(cur, markers)))) {
          root <- cur
          break
        }
      }
    }
  }
  if (!dir.exists(root)) {
    stop("Project root does not exist: ", root, call. = FALSE)
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

.pce_lock_dir <- function(path = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("PCE_LOCK_DIR", unset = "")
    if (!nzchar(path)) {
      path <- getOption("pce.lock_dir", "")
      if (is.null(path)) path <- ""
    }
    if (!nzchar(path)) path <- file.path(.pce_project_root(), "config")
  }
  if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path)) {
    stop("Lock directory must be a single non-empty path.", call. = FALSE)
  }
  path <- path.expand(path)
  if (!grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", path)) {
    path <- file.path(.pce_project_root(), path)
  }
  path
}

.pce_lock_paths <- function(path = NULL) {
  dir <- .pce_lock_dir(path)
  list(
    dir = dir,
    bea_line_lock = file.path(dir, "bea_line_lock.csv"),
    crosswalk_lock = file.path(dir, "crosswalk_lock.csv")
  )
}

.pce_lock_integer <- function(dt, columns, path) {
  for (column in columns) {
    x <- dt[[column]]
    numeric <- suppressWarnings(as.numeric(as.character(x)))
    integer <- suppressWarnings(as.integer(as.character(x)))
    if (anyNA(integer) || anyNA(numeric) || any(integer != numeric)) {
      stop(
        "Lockfile ", path, " has a non-integer `", column, "` value.",
        call. = FALSE
      )
    }
    data.table::set(dt, j = column, value = integer)
  }
  invisible(dt)
}

.pce_lock_logical <- function(dt, columns, path) {
  for (column in columns) {
    x <- dt[[column]]
    if (is.logical(x)) {
      if (anyNA(x)) {
        stop("Lockfile ", path, " has a missing `", column, "` value.",
             call. = FALSE)
      }
      next
    }
    if (is.numeric(x)) {
      if (anyNA(x) || !all(x %in% c(0, 1))) {
        stop("Lockfile ", path, " has an invalid `", column, "` value.",
             call. = FALSE)
      }
      data.table::set(dt, j = column, value = as.logical(x))
      next
    }
    text <- toupper(trimws(as.character(x)))
    if (anyNA(text) || !all(text %in% c("TRUE", "FALSE"))) {
      stop("Lockfile ", path, " has an invalid `", column, "` value.",
           call. = FALSE)
    }
    data.table::set(dt, j = column, value = text == "TRUE")
  }
  invisible(dt)
}

.pce_lock_text <- function(dt, columns, path, allow_na = FALSE) {
  for (column in columns) {
    x <- as.character(dt[[column]])
    if (!allow_na && anyNA(x)) {
      stop("Lockfile ", path, " has a missing `", column, "` value.",
           call. = FALSE)
    }
    if (allow_na) {
      x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
    } else if (any(!nzchar(trimws(x)))) {
      stop("Lockfile ", path, " has an empty `", column, "` value.",
           call. = FALSE)
    }
    data.table::set(dt, j = column, value = x)
  }
  invisible(dt)
}

.pce_read_lock_file <- function(path, required) {
  if (!file.exists(path)) {
    stop(
      "Required lockfile does not exist: ", path, "\n",
      "Run update_bea_line_locks(price, nominal) after reviewing the BEA ",
      "line set, then commit the regenerated locks.",
      call. = FALSE
    )
  }
  lock <- tryCatch(
    data.table::fread(path, data.table = TRUE, na.strings = c("NA", "")),
    error = function(e) {
      stop("Could not read lockfile ", path, ": ", conditionMessage(e),
           call. = FALSE)
    }
  )
  missing <- setdiff(required, names(lock))
  if (length(missing)) {
    stop(
      "Lockfile ", path, " is missing required column(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(names(lock))) {
    stop("Lockfile ", path, " has duplicated column names.", call. = FALSE)
  }
  lock[]
}

.pce_read_locks <- function(path = NULL) {
  paths <- .pce_lock_paths(path)
  list(
    paths = paths,
    bea_line_lock = .pce_read_lock_file(
      paths$bea_line_lock,
      c("table", "line_number", "description", "series_code")
    ),
    crosswalk_lock = .pce_read_lock_file(
      paths$crosswalk_lock,
      c(
        "component_id", "component_line", "line_number", "line_order",
        "price_line", "is_price_source", "component_description",
        "line_description", "price_series_code", "nominal_series_code",
        "housing", "market_based", "source_group", "dallas_name"
      )
    )
  )
}

.pce_validate_bea_line_lock <- function(lock, path) {
  .pce_lock_integer(lock, "line_number", path)
  .pce_lock_text(lock, c("table", "description", "series_code"), path)
  if (anyDuplicated(paste(lock$table, lock$line_number, sep = "|"))) {
    stop(
      "Lockfile ", path,
      " has duplicate table/line_number rows.",
      call. = FALSE
    )
  }
  tables <- sort(unique(lock$table))
  if (!identical(tables, c("U20404", "U20405"))) {
    stop(
      "Lockfile ", path, " must describe exactly U20404 and U20405; found: ",
      paste(tables, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(lock$line_number <= 0L)) {
    stop("Lockfile ", path, " has non-positive line numbers.", call. = FALSE)
  }
  data.table::setorder(lock, table, line_number)
  invisible(lock)
}

.pce_validate_crosswalk_lock_basic <- function(lock, path) {
  .pce_lock_integer(
    lock,
    c("line_number", "line_order", "price_line"),
    path
  )
  .pce_lock_logical(lock, c("is_price_source", "housing", "market_based"), path)
  .pce_lock_text(
    lock,
    c(
      "component_id", "component_line", "component_description",
      "line_description", "nominal_series_code", "source_group", "dallas_name"
    ),
    path
  )
  .pce_lock_text(lock, "price_series_code", path, allow_na = TRUE)
  if (nrow(lock) != .pce_expected_bea_lines) {
    stop(
      "Lockfile ", path, " has ", nrow(lock), " rows; expected ",
      .pce_expected_bea_lines, " expanded component lines.",
      call. = FALSE
    )
  }
  if (data.table::uniqueN(lock$component_id) != .pce_expected_components) {
    stop(
      "Lockfile ", path, " has ",
      data.table::uniqueN(lock$component_id),
      " components; expected ", .pce_expected_components, ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(lock$line_number)) {
    stop("Lockfile ", path, " assigns a BEA line more than once.", call. = FALSE)
  }
  if (anyDuplicated(paste(lock$component_id, lock$line_number, sep = "|"))) {
    stop("Lockfile ", path, " has duplicate component/line rows.", call. = FALSE)
  }
  if (!all(lock$source_group %in% .pce_source_groups)) {
    stop(
      "Lockfile ", path, " has unknown source_group value(s): ",
      paste(setdiff(unique(lock$source_group), .pce_source_groups),
            collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(lock$housing & !lock$market_based)) {
    stop(
      "Lockfile ", path, " flags a housing component as not market-based.",
      call. = FALSE
    )
  }
  if (any(lock$line_order <= 0L) || any(lock$price_line <= 0L)) {
    stop("Lockfile ", path, " has non-positive line_order or price_line.",
         call. = FALSE)
  }
  if (any(lock$is_price_source & is.na(lock$price_series_code))) {
    stop(
      "Lockfile ", path, " has a price source line without a price series code.",
      call. = FALSE
    )
  }
  if (anyNA(lock$nominal_series_code)) {
    stop("Lockfile ", path, " has a missing nominal series code.", call. = FALSE)
  }
  data.table::setorder(lock, component_id, line_order)
  invisible(lock)
}

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
  if (nrow(cw) != .pce_expected_components) {
    stop(
      "Internal PCE crosswalk must contain ", .pce_expected_components,
      " components.", call. = FALSE
    )
  }
  if (anyDuplicated(cw$component_id)) {
    stop("Internal PCE crosswalk has duplicated component_id values.", call. = FALSE)
  }
  if (anyNA(cw$price_line)) {
    stop("Internal PCE crosswalk has missing price_line values.", call. = FALSE)
  }
  if (!all(grepl("^pce_l[0-9]{3}(_l[0-9]{3})?$", cw$component_id))) {
    stop("Internal PCE crosswalk has an invalid component_id.", call. = FALSE)
  }
  if (anyNA(cw$housing) || anyNA(cw$market_based)) {
    stop("Internal PCE crosswalk has missing logical flags.", call. = FALSE)
  }
  if (any(cw$housing & !cw$market_based)) {
    stop("Internal PCE crosswalk flags housing as not market-based.", call. = FALSE)
  }
  if (!all(cw$source_group %in% .pce_source_groups)) {
    stop("Internal PCE crosswalk has an unknown source_group.", call. = FALSE)
  }
  if (any(!nzchar(trimws(cw$description))) ||
      any(!nzchar(trimws(cw$dallas_name)))) {
    stop("Internal PCE crosswalk has an empty description.", call. = FALSE)
  }

  line_sets <- strsplit(cw$line_number, "+", fixed = TRUE)
  line_int <- lapply(line_sets, function(x) {
    suppressWarnings(as.integer(x))
  })
  bad <- vapply(line_int, function(x) {
    anyNA(x) || !length(x) || any(x <= 0L) || anyDuplicated(x) > 0L
  }, logical(1))
  if (any(bad)) {
    stop(
      "Internal PCE crosswalk has an invalid line_number value for: ",
      paste(cw$component_id[bad], collapse = ", "), ".",
      call. = FALSE
    )
  }
  canonical <- vapply(line_int, paste, character(1), collapse = "+")
  if (any(canonical != cw$line_number)) {
    stop(
      "Internal PCE crosswalk line_number values must be ascending: ",
      paste(cw$component_id[canonical != cw$line_number], collapse = ", "), ".",
      call. = FALSE
    )
  }
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
    component_id, component_line = line_number, price_line, description,
    housing, market_based, source_group, dallas_name
  )]
  lm[, is_price_source := line_number == price_line]
  data.table::setcolorder(lm, c(
    "component_id", "component_line", "line_number", "line_order",
    "price_line", "is_price_source", "description", "housing",
    "market_based", "source_group", "dallas_name"
  ))
  data.table::setorder(lm, component_id, line_order)
  if (nrow(lm) != .pce_expected_bea_lines) {
    stop(
      "Internal PCE crosswalk expands to ", nrow(lm), " lines; expected ",
      .pce_expected_bea_lines, ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(lm$line_number)) {
    stop("Internal PCE crosswalk assigns a BEA line to more than one component.", call. = FALSE)
  }
  price_counts <- lm[, .(
    n_price = sum(is_price_source),
    n_price_line = data.table::uniqueN(price_line)
  ), by = component_id]
  if (any(price_counts$n_price != 1L) || any(price_counts$n_price_line != 1L)) {
    stop(
      "Every component must have exactly one price source line.",
      call. = FALSE
    )
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
  if (nrow(x) == 0L) {
    stop(sprintf("`%s` must contain at least one observation.", arg), call. = FALSE)
  }

  out <- x[, required, with = FALSE]

  line_number_int <- suppressWarnings(as.integer(as.character(out$line_number)))
  if (anyNA(line_number_int) || any(line_number_int <= 0L)) {
    stop(
      sprintf("`%s$line_number` must contain positive integers.", arg),
      call. = FALSE
    )
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
  out[, series_code := trimws(as.character(series_code))]
  out[, description := trimws(as.character(description))]

  if (anyNA(out$series_code) || any(!nzchar(out$series_code))) {
    stop(sprintf("`%s$series_code` must not contain missing values.", arg),
         call. = FALSE)
  }
  if (anyNA(out$description) || any(!nzchar(out$description))) {
    stop(sprintf("`%s$description` must not contain missing values.", arg),
         call. = FALSE)
  }

  duplicates <- out[, .N, by = .(line_number, date)][N > 1L]
  if (nrow(duplicates)) {
    stop(sprintf(
      "`%s` has duplicate line_number-date rows (first: line %s on %s).",
      arg, duplicates$line_number[1L], format(duplicates$date[1L])
    ), call. = FALSE)
  }

  metadata <- unique(out[, .(line_number, description, series_code)])
  conflicts <- metadata[, .(
    n_descriptions = data.table::uniqueN(description),
    n_codes = data.table::uniqueN(series_code)
  ), by = line_number][n_descriptions > 1L | n_codes > 1L]
  if (nrow(conflicts)) {
    first <- conflicts[1L]
    stop(sprintf(
      paste0(
        "`%s` has conflicting descriptions or series codes within line %d; ",
        "each BEA line must have one stable description and code."
      ),
      arg, first$line_number
    ), call. = FALSE)
  }
  out[]
}

.pce_line_inventory <- function(x, table) {
  meta <- unique(x[, .(line_number, description, series_code)])
  conflicts <- meta[, .(
    n_descriptions = data.table::uniqueN(description),
    n_codes = data.table::uniqueN(series_code)
  ), by = line_number][n_descriptions > 1L | n_codes > 1L]
  if (nrow(conflicts)) {
    stop(
      "Cannot build a ", table, " line lock: line ",
      conflicts$line_number[1L],
      " has more than one description or series code.",
      call. = FALSE
    )
  }
  data.table::setorder(meta, line_number)
  meta[, table := table]
  data.table::setcolorder(
    meta, c("table", "line_number", "description", "series_code")
  )
  meta[]
}

.pce_format_value <- function(x) {
  if (is.logical(x)) {
    return(ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE")))
  }
  ifelse(is.na(x), "NA", as.character(x))
}

.pce_line_lock_diff <- function(lock, current, table_id, max_show = 20L) {
  old <- lock[lock$table == table_id, .(line_number, description, series_code)]
  new <- current[
    current$table == table_id,
    .(line_number, description, series_code)
  ]
  if (!nrow(old) && !nrow(new)) return(character())
  if (!nrow(old)) {
    return(sprintf(
      "  %s has no locked lines; the current table has %d.",
      table_id, nrow(new)
    ))
  }
  if (!nrow(new)) {
    return(sprintf(
      "  %s has %d locked lines; the current table has none.",
      table_id, nrow(old)
    ))
  }

  common <- merge(
    old, new,
    by = "line_number", suffixes = c(".lock", ".current"), sort = FALSE
  )
  renamed <- common[description.lock != description.current]
  recoded <- common[series_code.lock != series_code.current]
  removed <- old[!old$line_number %in% new$line_number]
  added <- new[!new$line_number %in% old$line_number]

  issues <- character()
  add_examples <- function(dt, line_fun, noun) {
    if (!nrow(dt)) return(character())
    show <- utils::head(dt, max_show)
    out <- line_fun(show)
    if (nrow(dt) > nrow(show)) {
      out <- c(out, sprintf("  ... and %d more %s.", nrow(dt) - nrow(show), noun))
    }
    out
  }

  issues <- c(issues, add_examples(
    renamed,
    function(x) sprintf(
      "  Renamed line %d (%s): lock %s -> current %s.",
      x$line_number, table_id, x$description.lock, x$description.current
    ),
    "renamed line(s)"
  ))
  issues <- c(issues, add_examples(
    recoded,
    function(x) sprintf(
      "  Recoded line %d (%s): lock %s -> current %s.",
      x$line_number, table_id, x$series_code.lock, x$series_code.current
    ),
    "recoded line(s)"
  ))
  issues <- c(issues, add_examples(
    removed,
    function(x) sprintf(
      "  Removed line %d (%s): %s [%s].",
      x$line_number, table_id, x$description, x$series_code
    ),
    "removed line(s)"
  ))
  issues <- c(issues, add_examples(
    added,
    function(x) sprintf(
      "  Added line %d (%s): %s [%s].",
      x$line_number, table_id, x$description, x$series_code
    ),
    "added line(s)"
  ))

  if (nrow(removed) && nrow(added)) {
    renumbered <- merge(
      removed[, .(
        old_line = line_number,
        old_description = description,
        series_code
      )],
      added[, .(
        new_line = line_number,
        new_description = description,
        series_code
      )],
      by = "series_code", sort = FALSE
    )
    issues <- c(issues, add_examples(
      renumbered,
      function(x) sprintf(
        paste0(
          "  Possible renumbering (%s): line %d %s [%s] -> ",
          "line %d %s [%s]."
        ),
        table_id, x$old_line, x$old_description, x$series_code,
        x$new_line, x$new_description, x$series_code
      ),
      "possible renumbering pair(s)"
    ))
  }
  issues
}

.pce_assert_line_inventory <- function(lock, p, n, paths) {
  price_inventory <- .pce_line_inventory(p, "U20404")
  nominal_inventory <- .pce_line_inventory(n, "U20405")
  issues <- c(
    .pce_line_lock_diff(lock, price_inventory, "U20404"),
    .pce_line_lock_diff(lock, nominal_inventory, "U20405")
  )
  if (length(issues)) {
    stop(paste(c(
      paste0("BEA line inventory does not match the reviewed lockfile: ",
             paths$bea_line_lock),
      issues,
      "",
      "BEA line additions, removals, renumbering, renames, and series-code changes",
      "must be reviewed before the component panel is rebuilt.",
      "If the change is intended, run update_bea_line_locks(price, nominal) and",
      "commit the regenerated lockfiles."
    ), collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

.pce_validate_date_grid <- function(p, n) {
  dates_price <- sort(unique(p$date))
  dates_nominal <- sort(unique(n$date))
  if (!length(dates_price) || !length(dates_nominal)) {
    stop("`price` and `nominal` must contain at least one month.",
         call. = FALSE)
  }
  if (any(format(dates_price, "%d") != "01") ||
      any(format(dates_nominal, "%d") != "01")) {
    stop(
      "BEA monthly inputs must be dated on the first day of a calendar month.",
      call. = FALSE
    )
  }

  max_price <- max(dates_price)
  max_nominal <- max(dates_nominal)
  if (as.numeric(max_price) != as.numeric(max_nominal)) {
    stop(sprintf(
      paste0(
        "Strict max-date equality failed: `price` ends on %s but ",
        "`nominal` ends on %s."
      ),
      format(max_price), format(max_nominal)
    ), call. = FALSE)
  }
  if (length(dates_price) != length(dates_nominal) ||
      any(as.numeric(dates_price) != as.numeric(dates_nominal))) {
    only_price <- setdiff(dates_price, dates_nominal)
    only_nominal <- setdiff(dates_nominal, dates_price)
    stop(paste(c(
      "`price` and `nominal` must cover exactly the same months.",
      if (length(only_price)) {
        paste0("  Only in `price`: ",
               paste(format(head(only_price, 6L)), collapse = ", "))
      } else "",
      if (length(only_nominal)) {
        paste0("  Only in `nominal`: ",
               paste(format(head(only_nominal, 6L)), collapse = ", "))
      } else ""
    ), collapse = "\n"), call. = FALSE)
  }

  month_id <- .pce_month_index(dates_price)
  if (length(month_id) > 1L && any(diff(month_id) != 1L)) {
    at <- which(diff(month_id) != 1L)[1L]
    stop(sprintf(
      "Monthly coverage is not contiguous: a month is missing between %s and %s.",
      format(dates_price[at]), format(dates_price[at + 1L])
    ), call. = FALSE)
  }

  check_gaps <- function(x, label) {
    ordered <- x[order(line_number, date)]
    gaps <- ordered[, .(
      line_number,
      date,
      gap = c(NA_integer_, diff(.pce_month_index(date)))
    ), by = line_number]
    bad <- gaps[!is.na(gap) & gap != 1L]
    if (nrow(bad)) {
      stop(sprintf(
        paste0(
          "`%s` is not monthly-contiguous for line %d: a month is missing ",
          "before %s."
        ),
        label, bad$line_number[1L], format(bad$date[1L])
      ), call. = FALSE)
    }
    invisible(TRUE)
  }
  check_gaps(p, "price")
  check_gaps(n, "nominal")
  invisible(dates_price)
}

.pce_month_index <- function(x) {
  as.integer(format(x, "%Y")) * 12L + as.integer(format(x, "%m"))
}

.pce_validate_line_coverage <- function(x, dates, required, label, role) {
  required <- sort(unique(as.integer(required)))
  if (!length(required)) return(invisible(TRUE))
  have <- unique(x[line_number %in% required, .(line_number, date)])
  grid <- data.table::data.table(
    line_number = rep(required, each = length(dates)),
    date = rep(dates, times = length(required))
  )
  missing <- data.table::fsetdiff(grid, have)
  if (nrow(missing)) {
    stop(sprintf(
      paste0(
        "`%s` is missing %d month(s) for %s lines (first: line %d at %s). ",
        "Every %s line must cover the full date range."
      ),
      label, nrow(missing), role, missing$line_number[1L],
      format(missing$date[1L]), role
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.pce_check_identity <- function(n, spec,
                                tolerance = .pce_identity_tolerance) {
  lines <- c(spec$parent, spec$children)
  missing <- setdiff(lines, unique(n$line_number))
  if (length(missing)) {
    stop(sprintf(
      "Cannot check %s: nominal line(s) %s are missing.",
      spec$label, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  parent <- n[line_number == spec$parent, .(date, parent_value = value)]
  child <- n[line_number %in% spec$children, .(
    child_value = sum(value),
    child_lines = data.table::uniqueN(line_number)
  ), by = date]
  if (any(child$child_lines != length(spec$children))) {
    stop(sprintf(
      "Cannot check %s: one or more child lines have missing months.",
      spec$label
    ), call. = FALSE)
  }
  checked <- merge(parent, child, by = "date", all = TRUE, sort = TRUE)
  checked[, residual := parent_value - child_value]
  bad <- checked[is.na(residual) | abs(residual) > tolerance]
  if (nrow(bad)) {
    worst <- bad[which.max(abs(residual))]
    stop(sprintf(
      paste0(
        "Accounting identity failed: %s.\n",
        "  Largest absolute residual: %s (tolerance %s), first failing month: %s.\n",
        "  Failing months: %d of %d."
      ),
      spec$label,
      formatC(worst$residual, format = "f", big.mark = ",", digits = 0),
      formatC(tolerance, format = "f", big.mark = ",", digits = 0),
      format(worst$date),
      nrow(bad), nrow(checked)
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.pce_validate_accounting_identities <- function(n) {
  for (spec in .pce_identity_specs) {
    .pce_check_identity(n, spec)
  }
  invisible(TRUE)
}

.pce_validate_crosswalk_structure <- function(cw, lm, lock, bea_lock,
                                               lock_path) {
  issues <- character()
  add <- function(x) issues <<- c(issues, x)

  if (nrow(cw) != .pce_expected_components ||
      nrow(lm) != .pce_expected_bea_lines ||
      data.table::uniqueN(lm$line_number) != .pce_expected_bea_lines) {
    add(sprintf(
      "  Internal map expands to %d components and %d lines; expected %d and %d.",
      data.table::uniqueN(lm$component_id), nrow(lm),
      .pce_expected_components, .pce_expected_bea_lines
    ))
  }
  if (nrow(lock) != .pce_expected_bea_lines ||
      data.table::uniqueN(lock$component_id) != .pce_expected_components) {
    add(sprintf(
      "  Lock has %d rows and %d components; expected %d and %d.",
      nrow(lock), data.table::uniqueN(lock$component_id),
      .pce_expected_bea_lines, .pce_expected_components
    ))
  }

  expected <- lm[, .(
    component_id, component_line, line_number, line_order, price_line,
    is_price_source, component_description = description,
    housing, market_based, source_group, dallas_name
  )]
  expected_key <- paste(expected$component_id, expected$line_number, sep = "\r")
  lock_key <- paste(lock$component_id, lock$line_number, sep = "\r")

  missing <- expected[!expected_key %in% lock_key]
  if (nrow(missing)) {
    show <- utils::head(missing, 10L)
    add(sprintf(
      "  Missing lock row(s): component %s line %d.",
      show$component_id, show$line_number
    ))
    if (nrow(missing) > nrow(show)) {
      add(sprintf("  ... and %d more missing rows.", nrow(missing) - nrow(show)))
    }
  }
  extra <- lock[!lock_key %in% expected_key]
  if (nrow(extra)) {
    show <- utils::head(extra, 10L)
    add(sprintf(
      "  Unexpected lock row(s): component %s line %d.",
      show$component_id, show$line_number
    ))
    if (nrow(extra) > nrow(show)) {
      add(sprintf("  ... and %d more unexpected rows.", nrow(extra) - nrow(show)))
    }
  }

  common <- intersect(expected_key, lock_key)
  if (length(common)) {
    exp <- expected[match(common, expected_key)]
    cur <- lock[match(common, lock_key)]
    fields <- c(
      "component_line", "line_order", "price_line", "is_price_source",
      "component_description", "housing", "market_based", "source_group",
      "dallas_name"
    )
    for (field in fields) {
      a <- exp[[field]]
      b <- cur[[field]]
      same <- (is.na(a) & is.na(b)) |
        (!is.na(a) & !is.na(b) & as.character(a) == as.character(b))
      bad <- which(!same)
      if (length(bad)) {
        show <- utils::head(bad, 10L)
        add(sprintf(
          "  `%s` mismatch for component %s line %d: expected %s, lock has %s.",
          field, exp$component_id[show], exp$line_number[show],
          .pce_format_value(a[show]), .pce_format_value(b[show])
        ))
        if (length(bad) > length(show)) {
          add(sprintf("  ... and %d more `%s` mismatches.",
                      length(bad) - length(show), field))
        }
      }
    }
  }

  canonical <- lm[, .(
    component_line = component_line[1L],
    expected_line = paste(line_number, collapse = "+")
  ), by = component_id]
  bad <- canonical[component_line != expected_line]
  if (nrow(bad)) {
    add(sprintf(
      "  component_line %s should be %s for component %s.",
      bad$component_line, bad$expected_line, bad$component_id
    ))
  }

  bad <- lock[is_price_source & line_number != price_line]
  if (nrow(bad)) {
    add(sprintf(
      "  Price source line %d does not equal price_line %d for component %s.",
      bad$line_number, bad$price_line, bad$component_id
    ))
  }
  price_counts <- lock[, .(
    n_price = sum(is_price_source),
    n_price_lines = data.table::uniqueN(price_line)
  ), by = component_id]
  bad <- price_counts[n_price != 1L | n_price_lines != 1L]
  if (nrow(bad)) {
    add(sprintf(
      "  Component %s has %d price source rows; expected exactly one.",
      bad$component_id, bad$n_price
    ))
  }
  bad <- lock[, .(
    ok = identical(line_order, seq_len(.N))
  ), by = component_id][ok == FALSE]
  if (nrow(bad)) {
    add(sprintf("  line_order is not contiguous for component %s.", bad$component_id))
  }

  flag_lines <- sort(unique(lock[housing == TRUE, line_number]))
  if (!identical(flag_lines, .pce_housing_lines)) {
    add(sprintf(
      "  Housing flag covers line(s) %s; expected %s.",
      paste(flag_lines, collapse = ", "),
      paste(.pce_housing_lines, collapse = ", ")
    ))
  }
  n_housing <- data.table::uniqueN(lock[housing == TRUE, component_id])
  if (n_housing != .pce_expected_housing_components) {
    add(sprintf(
      "  Housing flag covers %d components; expected %d.",
      n_housing, .pce_expected_housing_components
    ))
  }
  n_market <- data.table::uniqueN(lock[market_based == TRUE, component_id])
  if (n_market != .pce_expected_market_components) {
    add(sprintf(
      "  Market-based flag covers %d components; expected %d.",
      n_market, .pce_expected_market_components
    ))
  }

  nominal_lock <- bea_lock[table == "U20405"]
  price_lock <- bea_lock[table == "U20404"]
  nominal_index <- match(lock$line_number, nominal_lock$line_number)
  if (anyNA(nominal_index)) {
    add(sprintf(
      "  Crosswalk line(s) %s are absent from the nominal line lock.",
      paste(lock$line_number[is.na(nominal_index)], collapse = ", ")
    ))
  } else {
    bad <- which(
      lock$line_description != nominal_lock$description[nominal_index] |
        lock$nominal_series_code != nominal_lock$series_code[nominal_index]
    )
    if (length(bad)) {
      show <- utils::head(bad, 10L)
      add(sprintf(
        paste0(
          "  Nominal metadata for line %d differs from the line lock: ",
          "lock has %s [%s], crosswalk has %s [%s]."
        ),
        lock$line_number[show],
        nominal_lock$description[nominal_index[show]],
        nominal_lock$series_code[nominal_index[show]],
        lock$line_description[show],
        lock$nominal_series_code[show]
      ))
    }
  }

  price_index <- match(lock$line_number, price_lock$line_number)
  has_price <- !is.na(price_index)
  if (any(lock$is_price_source & !has_price)) {
    add(sprintf(
      "  Price source line(s) %s are absent from the price line lock.",
      paste(lock$line_number[lock$is_price_source & !has_price],
            collapse = ", ")
    ))
  }
  if (any(has_price)) {
    bad <- which(
      has_price & (
        is.na(lock$price_series_code) |
          lock$price_series_code != price_lock$series_code[price_index] |
          lock$line_description != price_lock$description[price_index]
      )
    )
    if (length(bad)) {
      show <- utils::head(bad, 10L)
      add(sprintf(
        paste0(
          "  Price metadata for line %d differs from the line lock: ",
          "lock has %s [%s], crosswalk has %s [%s]."
        ),
        lock$line_number[show],
        price_lock$description[price_index[show]],
        price_lock$series_code[price_index[show]],
        lock$line_description[show],
        .pce_format_value(lock$price_series_code[show])
      ))
    }
  }
  if (any(!has_price & !is.na(lock$price_series_code))) {
    add(sprintf(
      paste0(
        "  Crosswalk has a price series code but the line lock has no price ",
        "line for %s."
      ),
      paste(lock$line_number[!has_price & !is.na(lock$price_series_code)],
            collapse = ", ")
    ))
  }

  if (length(issues)) {
    show <- utils::head(issues, 80L)
    if (length(issues) > length(show)) {
      show <- c(show, sprintf("  ... and %d more lock problem(s).",
                              length(issues) - length(show)))
    }
    stop(paste(c(
      paste0("Crosswalk lock does not match the confirmed component mapping (",
             lock_path, ")."),
      show,
      "",
      "Review the component mapping and lockfiles. If the change is intended,",
      "run update_bea_line_locks(price, nominal) and commit the new locks."
    ), collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

.pce_validate_crosswalk_data <- function(lock, p, n, lock_path) {
  issues <- character()
  add <- function(x) issues <<- c(issues, x)

  nominal_meta <- unique(n[, .(line_number, description, series_code)])
  price_meta <- unique(p[, .(line_number, description, series_code)])

  nominal_index <- match(lock$line_number, nominal_meta$line_number)
  if (anyNA(nominal_index)) {
    add(sprintf(
      "  Mapped line(s) %s are missing from `nominal`.",
      paste(lock$line_number[is.na(nominal_index)], collapse = ", ")
    ))
  } else {
    bad <- which(
      lock$line_description != nominal_meta$description[nominal_index] |
        lock$nominal_series_code != nominal_meta$series_code[nominal_index]
    )
    if (length(bad)) {
      show <- utils::head(bad, 10L)
      add(sprintf(
        paste0(
          "  Nominal line %d: lock expects %s [%s], current data has %s [%s]."
        ),
        lock$line_number[show],
        lock$line_description[show], lock$nominal_series_code[show],
        nominal_meta$description[nominal_index[show]],
        nominal_meta$series_code[nominal_index[show]]
      ))
    }
  }

  price_index <- match(lock$line_number, price_meta$line_number)
  has_price <- !is.na(price_index)
  if (any(lock$is_price_source & !has_price)) {
    add(sprintf(
      "  Price source line(s) %s are missing from `price`.",
      paste(lock$line_number[lock$is_price_source & !has_price],
            collapse = ", ")
    ))
  }
  if (any(has_price)) {
    bad <- which(
      has_price & (
        is.na(lock$price_series_code) |
          lock$price_series_code != price_meta$series_code[price_index] |
          lock$line_description != price_meta$description[price_index]
      )
    )
    if (length(bad)) {
      show <- utils::head(bad, 10L)
      add(sprintf(
        paste0(
          "  Price line %d: lock expects %s [%s], current data has %s [%s]."
        ),
        lock$line_number[show],
        lock$line_description[show],
        .pce_format_value(lock$price_series_code[show]),
        price_meta$description[price_index[show]],
        price_meta$series_code[price_index[show]]
      ))
    }
  }
  if (any(!has_price & !is.na(lock$price_series_code))) {
    add(sprintf(
      paste0(
        "  Crosswalk has a price series code for line %s but current `price` ",
        "has no such line."
      ),
      paste(lock$line_number[!has_price & !is.na(lock$price_series_code)],
            collapse = ", ")
    ))
  }

  line_counts <- lock[, .N, by = component_id]
  single <- lock[component_id %in% line_counts[N == 1L]$component_id]
  normalize <- function(x) gsub("[[:space:]]+", " ", tolower(trimws(x)))
  compatible <- startsWith(
    normalize(single$line_description), normalize(single$component_description)
  ) | startsWith(
    normalize(single$component_description), normalize(single$line_description)
  )
  if (any(!compatible)) {
    bad <- single[!compatible]
    show <- utils::head(bad, 10L)
    add(sprintf(
      paste0(
        "  Component %s description is not compatible with BEA line %d ",
        "description (%s vs %s)."
      ),
      show$component_id, show$line_number,
      show$component_description, show$line_description
    ))
  }

  if (length(issues)) {
    show <- utils::head(issues, 80L)
    if (length(issues) > length(show)) {
      show <- c(show, sprintf("  ... and %d more mapped-line problem(s).",
                              length(issues) - length(show)))
    }
    stop(paste(c(
      paste0("Mapped component descriptions or series codes do not match ",
             "current BEA data (", lock_path, ")."),
      show,
      "",
      "Review the mapping. If the BEA change is intended, update the mapping",
      "and run update_bea_line_locks(price, nominal), then commit the locks."
    ), collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

.pce_validate_panel <- function(panel, n, lock, dates) {
  if (nrow(panel) != .pce_expected_components * length(dates)) {
    stop(sprintf(
      "Panel has %d rows; expected %d components x %d months = %d.",
      nrow(panel), .pce_expected_components, length(dates),
      .pce_expected_components * length(dates)
    ), call. = FALSE)
  }
  if (data.table::uniqueN(panel$component_id) != .pce_expected_components) {
    stop("Panel does not contain the expected 192 components.", call. = FALSE)
  }
  component_months <- panel[, .N, by = component_id]
  date_components <- panel[, .N, by = date]
  if (any(component_months$N != length(dates)) ||
      any(date_components$N != .pce_expected_components)) {
    stop(
      "Panel is not a complete component-by-month rectangle.",
      call. = FALSE
    )
  }
  if (anyNA(panel$price) || anyNA(panel$nominal)) {
    stop("Panel contains missing price or nominal values.", call. = FALSE)
  }
  for (flag in c("housing", "market_based")) {
    if (!is.logical(panel[[flag]]) || anyNA(panel[[flag]])) {
      stop(sprintf("Panel `%s` must be a non-missing logical column.", flag),
           call. = FALSE)
    }
  }
  if (any(panel$housing & !panel$market_based)) {
    stop("Panel flags a housing component as not market-based.", call. = FALSE)
  }
  if (!all(panel$source_group %in% .pce_source_groups)) {
    stop("Panel has an unknown source_group value.", call. = FALSE)
  }
  panel_dates <- sort(unique(panel$date))
  if (length(panel_dates) != length(dates) ||
      any(as.numeric(panel_dates) != as.numeric(dates))) {
    stop("Panel dates do not match the complete BEA monthly grid.",
         call. = FALSE)
  }
  if (as.numeric(max(panel$date)) != as.numeric(max(n$date))) {
    stop(sprintf(
      paste0(
        "Strict max-date equality failed: panel ends on %s but nominal BEA ",
        "data end on %s."
      ),
      format(max(panel$date)), format(max(n$date))
    ), call. = FALSE)
  }

  lock_info <- unique(lock[, .(
    component_id, component_line, component_description,
    housing, market_based, source_group
  )])
  panel_info <- unique(panel[, .(
    component_id,
    panel_line = line_number,
    panel_description = description,
    panel_housing = housing,
    panel_market_based = market_based,
    panel_source_group = source_group
  )])
  info <- merge(lock_info, panel_info, by = "component_id", all = TRUE,
                sort = FALSE)
  if (nrow(info) != .pce_expected_components || anyNA(info$component_id) ||
      anyNA(info$component_line) || anyNA(info$panel_line) ||
      anyNA(info$component_description) || anyNA(info$panel_description) ||
      anyNA(info$housing) || anyNA(info$panel_housing) ||
      anyNA(info$market_based) || anyNA(info$panel_market_based) ||
      anyNA(info$source_group) || anyNA(info$panel_source_group)) {
    stop("Panel components do not match the crosswalk lock.", call. = FALSE)
  }
  bad <- info[
    component_line != panel_line |
      component_description != panel_description |
      housing != panel_housing |
      market_based != panel_market_based |
      source_group != panel_source_group
  ]
  if (nrow(bad)) {
    first <- bad[1L]
    stop(sprintf(
      paste0(
        "Panel metadata does not match the crosswalk lock for component %s ",
        "(line %s vs %s, description/flags differ)."
      ),
      first$component_id, first$component_line, first$panel_line
    ), call. = FALSE)
  }

  expected_codes <- lock[, {
    price_code <- paste(
      unique(price_series_code[is_price_source]), collapse = "+"
    )
    nominal_code <- paste(unique(nominal_series_code), collapse = "+")
    list(
      expected_series_code = if (identical(price_code, nominal_code)) {
        price_code
      } else {
        paste(price_code, nominal_code, sep = "|")
      }
    )
  }, by = component_id]
  actual_codes <- unique(panel[, .(component_id, series_code)])
  codes <- merge(expected_codes, actual_codes, by = "component_id",
                 all = TRUE, sort = FALSE)
  bad <- codes[
    is.na(series_code) | is.na(expected_series_code) |
      series_code != expected_series_code
  ]
  if (nrow(bad)) {
    first <- bad[1L]
    stop(sprintf(
      paste0(
        "Component series code for %s is %s; the crosswalk lock expects %s. ",
        "Merged lines must keep the price|nominal code construction."
      ),
      first$component_id,
      .pce_format_value(first$series_code),
      .pce_format_value(first$expected_series_code)
    ), call. = FALSE)
  }

  required <- c(1L, .pce_excluded_lines)
  missing <- setdiff(required, unique(n$line_number))
  if (length(missing)) {
    stop(sprintf(
      "Cannot check PCE coverage: nominal line(s) %s are missing.",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  total <- n[line_number == 1L, .(date, pce_total = value)]
  excluded <- n[line_number %in% .pce_excluded_lines, .(
    excluded_nominal = sum(value),
    n_excluded = data.table::uniqueN(line_number)
  ), by = date]
  if (any(excluded$n_excluded != length(.pce_excluded_lines))) {
    stop("Cannot check PCE coverage: an excluded adjustment line is missing.",
         call. = FALSE)
  }
  aggregate <- panel[, .(
    component_nominal = sum(nominal),
    market_nominal = sum(nominal[market_based]),
    housing_nominal = sum(nominal[housing]),
    nonmarket_nominal = sum(nominal[!market_based])
  ), by = date]
  if (any(aggregate$component_nominal <= 0)) {
    stop("Panel nominal PCE is not strictly positive.", call. = FALSE)
  }

  coverage <- merge(
    merge(total, excluded, by = "date", all = TRUE),
    aggregate, by = "date", all = TRUE
  )
  coverage[, gap := component_nominal + excluded_nominal - pce_total]
  tolerance <- .pce_coverage_tolerance + 1e-6 * abs(coverage$pce_total)
  bad <- coverage[is.na(gap) | abs(gap) > tolerance]
  if (nrow(bad)) {
    worst <- bad[which.max(abs(gap))]
    stop(sprintf(
      paste0(
        "Panel PCE coverage failed: sum(component nominal) + lines 145 + 333 ",
        "differs from total PCE by %s in %s (tolerance %s).\n",
        "  Failing months: %d of %d."
      ),
      formatC(worst$gap, format = "f", big.mark = ",", digits = 0),
      format(worst$date),
      formatC(max(tolerance), format = "f", big.mark = ",", digits = 0),
      nrow(bad), nrow(coverage)
    ), call. = FALSE)
  }

  raw_housing <- n[line_number %in% .pce_housing_lines, .(
    raw_housing = sum(value),
    n_housing_lines = data.table::uniqueN(line_number)
  ), by = date]
  if (any(raw_housing$n_housing_lines != length(.pce_housing_lines))) {
    stop("Cannot check the housing aggregate: a housing line is missing.",
         call. = FALSE)
  }
  coverage <- merge(coverage, raw_housing, by = "date", all.x = TRUE,
                    sort = FALSE)
  bad <- coverage[
    is.na(raw_housing) |
      abs(housing_nominal - raw_housing) > .pce_identity_tolerance
  ]
  if (nrow(bad)) {
    worst <- bad[which.max(abs(housing_nominal - raw_housing))]
    stop(sprintf(
      paste0(
        "Housing aggregate check failed: panel housing nominal differs from ",
        "BEA lines 155 + 156 + 161 + 162 by %s in %s (tolerance %s)."
      ),
      formatC(worst$housing_nominal - worst$raw_housing,
              format = "f", big.mark = ",", digits = 0),
      format(worst$date),
      .pce_identity_tolerance
    ), call. = FALSE)
  }

  coverage[, `:=`(
    market_share = market_nominal / component_nominal,
    housing_share = housing_nominal / component_nominal
  )]
  bad <- coverage[
    market_share < .pce_market_share_range[1L] |
      market_share > .pce_market_share_range[2L] |
      housing_share < .pce_housing_share_range[1L] |
      housing_share > .pce_housing_share_range[2L] |
      housing_nominal > market_nominal |
      nonmarket_nominal <= 0
  ]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "Market/housing aggregate shares are outside practical bounds: ",
        "market %.3f-%.3f, housing %.3f-%.3f. Expected market [%.2f, %.2f] ",
        "and housing [%.2f, %.2f]."
      ),
      min(coverage$market_share), max(coverage$market_share),
      min(coverage$housing_share), max(coverage$housing_share),
      .pce_market_share_range[1L], .pce_market_share_range[2L],
      .pce_housing_share_range[1L], .pce_housing_share_range[2L]
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Build the long component panel. `price` and `nominal` must be long BEA
# tables with line_number, series_code, description, date, and value. With
# strict = TRUE every mapped line must be present; strict = FALSE drops
# components with missing lines and allows NA component-date cells.
# `series_code` is the designated price line's code; when the price and nominal
# codes differ, both are returned as "price|nominal". Nominal codes for a
# multi-line component are joined with "+". Both BEA inputs are checked against
# the tracked line and crosswalk locks before the panel is built.
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
  n_all <- data.table::copy(n)

  locks <- .pce_read_locks()
  paths <- locks$paths
  .pce_validate_bea_line_lock(locks$bea_line_lock, paths$bea_line_lock)
  .pce_validate_crosswalk_lock_basic(
    locks$crosswalk_lock, paths$crosswalk_lock
  )
  .pce_assert_line_inventory(locks$bea_line_lock, p, n, paths)
  .pce_validate_crosswalk_structure(
    cw, lm, locks$crosswalk_lock, locks$bea_line_lock,
    paths$crosswalk_lock
  )

  dates <- .pce_validate_date_grid(p, n)
  required_price <- sort(unique(lm[is_price_source == TRUE, line_number]))
  required_nominal <- sort(unique(lm$line_number))
  .pce_validate_line_coverage(
    p, dates, required_price, "price", "price source"
  )
  .pce_validate_line_coverage(
    n, dates, unique(c(required_nominal, 1L, .pce_excluded_lines)),
    "nominal", "mapped, total, or adjustment"
  )
  .pce_validate_crosswalk_data(
    locks$crosswalk_lock, p, n, paths$crosswalk_lock
  )
  .pce_validate_accounting_identities(n)

  missing_price <- setdiff(required_price, unique(p$line_number))
  missing_nominal <- setdiff(required_nominal, unique(n$line_number))
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

  if (!nrow(incomplete)) {
    .pce_validate_panel(out, n_all, locks$crosswalk_lock, dates)
  }

  attr(out, "n_components") <- nrow(comps)
  attr(out, "n_bea_lines") <- nrow(lm)
  attr(out, "crosswalk") <- "BEA U20404/U20405; Dallas Fed 177 baseline; 192-component reconstruction; BEA market-based composition"
  attr(out, "excluded_lines") <- .pce_excluded_lines
  attr(out, "excluded_line_reasons") <- .pce_excluded_line_reasons
  out[]
}

# Deliberately accept the current BEA line inventory and component mapping.
#
# This is never called automatically. It regenerates the two tracked lockfiles
# after manual review of a line addition, removal, renumbering, rename, or
# series-code change:
#   config/bea_line_lock.csv     all U20404 and U20405 lines
#   config/crosswalk_lock.csv    all 194 expanded component lines
#
# `path` is a directory, resolved against the project root when relative. With
# `dry_run = TRUE`, changes are reported without writing. Returns the generated
# lock tables invisibly.
update_bea_line_locks <- function(price, nominal, path = "config",
                                  dry_run = FALSE) {
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("`dry_run` must be a single non-missing logical (TRUE or FALSE).",
         call. = FALSE)
  }

  cw <- .pce_crosswalk()
  lm <- .pce_expand_lines(cw)
  p <- .pce_prepare_input(price, "price")
  n <- .pce_prepare_input(nominal, "nominal")
  dates <- .pce_validate_date_grid(p, n)
  .pce_validate_line_coverage(
    p, dates,
    sort(unique(lm[is_price_source == TRUE, line_number])),
    "price", "price source"
  )
  .pce_validate_line_coverage(
    n, dates,
    unique(c(lm$line_number, 1L, .pce_excluded_lines)),
    "nominal", "mapped, total, or adjustment"
  )

  bea_line_lock <- data.table::rbindlist(list(
    .pce_line_inventory(p, "U20404"),
    .pce_line_inventory(n, "U20405")
  ), use.names = TRUE)
  data.table::setorder(bea_line_lock, table, line_number)

  crosswalk_lock <- .pce_build_crosswalk_lock(cw, lm, p, n)

  paths <- .pce_lock_paths(path)
  .pce_validate_bea_line_lock(bea_line_lock, paths$bea_line_lock)
  .pce_validate_crosswalk_lock_basic(
    crosswalk_lock, paths$crosswalk_lock
  )
  .pce_validate_crosswalk_structure(
    cw, lm, crosswalk_lock, bea_line_lock, paths$crosswalk_lock
  )
  .pce_validate_crosswalk_data(
    crosswalk_lock, p, n, paths$crosswalk_lock
  )

  old_lock <- tryCatch(
    .pce_read_lock_file(
      paths$bea_line_lock,
      c("table", "line_number", "description", "series_code")
    ),
    error = function(e) NULL
  )
  if (is.null(old_lock)) {
    message("No readable existing BEA line lock; the current line inventory ",
            "will be recorded.")
  } else {
    price_inventory <- .pce_line_inventory(p, "U20404")
    nominal_inventory <- .pce_line_inventory(n, "U20405")
    changes <- c(
      .pce_line_lock_diff(old_lock, price_inventory, "U20404"),
      .pce_line_lock_diff(old_lock, nominal_inventory, "U20405")
    )
    if (length(changes)) {
      message("BEA line inventory changes to accept:")
      message(paste(changes, collapse = "\n"))
    } else {
      message("BEA line inventory is unchanged.")
    }
  }
  message(sprintf(
    "Crosswalk lock: %d components, %d expanded lines, data through %s.",
    data.table::uniqueN(crosswalk_lock$component_id),
    nrow(crosswalk_lock), format(max(dates))
  ))

  if (dry_run) {
    message("Dry run only; lockfiles were not written.")
  } else {
    .pce_write_lock(bea_line_lock, paths$bea_line_lock)
    .pce_write_lock(crosswalk_lock, paths$crosswalk_lock)
    message("Wrote ", paths$bea_line_lock, " and ", paths$crosswalk_lock, ".")
  }

  invisible(list(
    bea_line_lock = bea_line_lock,
    crosswalk_lock = crosswalk_lock,
    bea_line_lock_path = paths$bea_line_lock,
    crosswalk_lock_path = paths$crosswalk_lock
  ))
}

.pce_build_crosswalk_lock <- function(cw, lm, p, n) {
  price_inventory <- .pce_line_inventory(p, "U20404")
  nominal_inventory <- .pce_line_inventory(n, "U20405")

  out <- merge(
    lm,
    price_inventory[, .(
      line_number,
      price_description = description,
      price_series_code = series_code
    )],
    by = "line_number", all.x = TRUE, sort = FALSE
  )
  out <- merge(
    out,
    nominal_inventory[, .(
      line_number,
      line_description = description,
      nominal_series_code = series_code
    )],
    by = "line_number", all.x = TRUE, sort = FALSE
  )

  if (anyNA(out$line_description) || anyNA(out$nominal_series_code)) {
    missing <- out[is.na(line_description) | is.na(nominal_series_code),
                   line_number]
    stop(
      "Cannot build crosswalk lock: mapped nominal line(s) ",
      paste(missing, collapse = ", "),
      " are missing or have conflicting metadata.",
      call. = FALSE
    )
  }
  if (any(out$is_price_source &
          (is.na(out$price_series_code) | is.na(out$price_description)))) {
    missing <- out[is_price_source &
                     (is.na(price_series_code) | is.na(price_description)),
                   line_number]
    stop(
      "Cannot build crosswalk lock: price source line(s) ",
      paste(missing, collapse = ", "),
      " are missing from the price table.",
      call. = FALSE
    )
  }
  has_price <- !is.na(out$price_description)
  if (any(has_price & out$price_description != out$line_description)) {
    bad <- out[has_price & price_description != line_description, line_number]
    stop(
      "Cannot build crosswalk lock: line(s) ",
      paste(bad, collapse = ", "),
      " have different price and nominal descriptions.",
      call. = FALSE
    )
  }

  out[, component_description := description]
  out[, description := NULL]
  out[, price_description := NULL]
  data.table::setcolorder(out, c(
    "component_id", "component_line", "line_number", "line_order",
    "price_line", "is_price_source", "component_description",
    "line_description", "price_series_code", "nominal_series_code",
    "housing", "market_based", "source_group", "dallas_name"
  ))
  data.table::setorder(out, component_id, line_order)
  out[]
}

.pce_write_lock <- function(dt, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(path))) {
    stop("Could not create lock directory: ", dirname(path), call. = FALSE)
  }
  tmp <- tempfile(
    paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(
    data.table::fwrite(dt, tmp, na = "NA"),
    error = function(e) {
      stop("Could not write lockfile ", path, ": ", conditionMessage(e),
           call. = FALSE)
    }
  )
  if (!file.copy(tmp, path, overwrite = TRUE)) {
    stop("Could not replace lockfile ", path, call. = FALSE)
  }
  invisible(path)
}
