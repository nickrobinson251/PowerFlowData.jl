var documenterSearchIndex = {"docs":
[{"location":"","page":"Home","title":"Home","text":"CurrentModule = PowerFlowData","category":"page"},{"location":"#PowerFlowData","page":"Home","title":"PowerFlowData","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"PowerFlowData.jl provides a parser for PSS/E-format .raw Power Flow Data Files.","category":"page"},{"location":"","page":"Home","title":"Home","text":"To read a .raw file, use parse_network:","category":"page"},{"location":"","page":"Home","title":"Home","text":"parse_network(\"file.raw\")","category":"page"},{"location":"","page":"Home","title":"Home","text":"This will return a Network object, which contains the data parsed into dedicated structures matching the PSS/E-format specification.","category":"page"},{"location":"#Example","page":"Home","title":"Example","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Usually your data will be in a file, and you'd read it with parse_network(\"file.raw\"), but here we'll pass in the data directly, to show how it matches to the output:","category":"page"},{"location":"","page":"Home","title":"Home","text":"using PowerFlowData, DataFrames\ndata = IOBuffer(\"\"\"\n    0,   100.00          / PSS/E-29.3    WED, SEP 15 2021  21:04\n    SE SNAPSHOT 15-09-2021 PEAK CASE 18:00\n    FULL COPY OF SYNTHETIC\n         1,'AAA    3    ', 111.0000,4,     0.000,     0.000, 327,   1,0.00000,   0.0000,   1\n    222222,'PRPR C D    ',  42.0000,1,     0.000,     0.000, 694,  24,1.11117,  20.0606,   7\n    0 / END OF BUS DATA, BEGIN LOAD DATA\n    \"\"\"\n);\nnetwork = parse_network(data);\nNamedTuple(network.caseid)  # Case Identification data is a single row.\nDataFrame(network.buses)    # Bus data, and all other data, is a table.","category":"page"},{"location":"#API","page":"Home","title":"API","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"parse_network","category":"page"},{"location":"#PowerFlowData.parse_network","page":"Home","title":"PowerFlowData.parse_network","text":"parse_network(source) -> Network\n\nRead a PSS/E-format .raw Power Flow Data file and return a Network object.\n\n\n\n\n\n","category":"function"},{"location":"","page":"Home","title":"Home","text":"Network\nCaseID\nBuses\nLoads\nGenerators\nBranches","category":"page"},{"location":"#PowerFlowData.Network","page":"Home","title":"PowerFlowData.Network","text":"Network\n\nRepresentation of a power network.\n\nThe PSS/E data format comprises 16 data categories of network and equipment elements, each of which requires a particular type of data.\n\nSimilarly, a Network stores the data from each category in its own dedicated structure.\n\nCurrently supported are:\n\nCaseID\nBuses\nLoads\nGenerators\nBranches\n\nCaseID data is a single row (in the Tables.jl-sense). You can access it like network.caseid and interact with it like a NamedTuple, or even convert it to a NamedTuple with NamedTuple(caseid).\n\nAll other records (buses, loads, etc.) can be accessed also via the fields, for example network.buses, and each is returned as lightweight table structure (again, in the Tables.jl-sense). That is, all structures implement the Tables.jl interface, so can be passed to any valid sink, such as a DataFrame like DataFrame(network.buses).\n\nFor more info on working with tables see Tables.jl, and for common table operations see TableOperations.jl.\n\nFields\n\ncaseid::CaseID\nCase identification data.\nbuses::Buses\nBus records.\nloads::Loads\nLoad records.\ngenerators::Generators\nGenerator records.\nbranches::Branches\nNon-transformer Branch records.\n\n\n\n\n\n","category":"type"},{"location":"#PowerFlowData.CaseID","page":"Home","title":"PowerFlowData.CaseID","text":"struct CaseID <: Tables.AbstractRow\n\nFields\n\nic::Int64\nIC Change code: 0 - base case (i.e., clear the working case before adding data to it). 1 - add data to the working case.\n\nsbase::Float64\nSystem base MVA.\n\n\n\n\n\n","category":"type"},{"location":"#PowerFlowData.Buses","page":"Home","title":"PowerFlowData.Buses","text":"struct Buses <: PowerFlowData.Records\n\nEach network bus to be represented in PSS/E is introduced through a bus data record. Each bus data record includes not only data for the basic bus properties but also includes information on an optionally connected shunt admittance to ground. That admittance can represent a shunt capacitor or a shunt reactor (both with or without a real component) or a shunt resistor. It must not represent line connected admittance, loads, line charging or transformer magnetizing impedance, all of which are entered in other data categories.\n\nFields\n\ni::Vector{Int64}\nBus number (1 to 999997).\nname::Vector{WeakRefStrings.String15}\nAlphanumeric identifier assigned to bus \"I\". The name may be up to twelve characters and must be enclosed in single quotes. NAME may contain any combination of blanks, uppercase letters, numbers and special characters, but the first character must not be a minus sign.\n\nbasekv::Vector{Float64}\nBus base voltage; entered in kV.\nide::Vector{Int64}\nBus type code: 1 - load bus or other bus without any generator boundary condition. 2 - generator or plant bus either regulating voltage or with a fixed reactive power (Mvar). A generator that reaches its reactive power limit will no longer control voltage but rather hold reactive power at its limit. 3 - swing bus or slack bus. It has no power or reactive limits and regulates voltage at a fixed reference angle. 4 - disconnected or isolated bus.\n\ngl::Vector{Float64}\nActive component of shunt admittance to ground; entered in MW at one per unit voltage. GL should not include any resistive admittance load, which is entered as part of load data.\n\nbl::Vector{Float64}\nReactive component of shunt admittance to ground; entered in Mvar at one per unit voltage. BL should not include any reactive impedance load, which is entered as part of load data; line charging and line connected shunts, which are entered as part of non-transformer branch data; or transformer magnetizing admittance, which is entered as part of transformer data. BL is positive for a capacitor, and negative for a reactor or an inductive load.\n\narea::Vector{Int64}\nArea number. 1 through the maximum number of areas at the current size level.\nzone::Vector{Int64}\nZone number. 1 through the maximum number of zones at the current size level.\nvm::Vector{Float64}\nBus voltage magnitude; entered in pu.\nva::Vector{Float64}\nBus voltage phase angle; entered in degrees.\nowner::Vector{Int64}\nOwner number. 1 through the maximum number of owners at the current size level.\n\n\n\n\n\n","category":"type"},{"location":"#PowerFlowData.Loads","page":"Home","title":"PowerFlowData.Loads","text":"struct Loads <: PowerFlowData.Records\n\nEach network bus at which a load is to be represented must be specified in at least one load data record. If multiple loads are to be represented at a bus, they must be individually identified in a load data record for the bus with a different load identifier. Each load at a bus can be a mixture of loads with different characteristics.\n\nFields\n\ni::Vector{Int64}\nBuses number, or extended buses name enclosed in single quotes.\nid::Vector{WeakRefStrings.String3}\nOne- or two-character uppercase non blank alphanumeric load identifier used to distinguish among multiple loads at bus \"I\". It is recommended that, at buses for which a single load is present, the load be designated as having the load identifier '1'.\n\nstatus::Vector{Bool}\nInitial load status of one for in-service and zero for out-of-service.\narea::Vector{Int64}\nArea to which the load is assigned (1 through the maximum number of areas at the current size level).\nzone::Vector{Float64}\nZone to which the load is assigned (1 through the maximum number of zones at the current size level).\npl::Vector{Float64}\nActive power component of constant MVA load; entered in MW.\nql::Vector{Float64}\nReactive power component of constant MVA load; entered in Mvar.\nip::Vector{Float64}\nActive power component of constant current load; entered in MW at one per unit voltage.\niq::Vector{Float64}\nReactive power component of constant current load; entered in Mvar at one per unit voltage.\nyp::Vector{Float64}\nActive power component of constant admittance load; entered in MW at one per unit voltage.\nyq::Vector{Float64}\nReactive power component of constant admittance load; entered in Mvar at one per unit voltage. YQ is a negative quantity for an inductive load and positive for a capacitive load.\n\nowner::Vector{Int64}\nOwner to which the load is assigned (1 through the maximum number of owners at the current size level).\n\n\n\n\n\n","category":"type"},{"location":"#PowerFlowData.Generators","page":"Home","title":"PowerFlowData.Generators","text":"struct Generators <: PowerFlowData.Records\n\nEach network bus to be represented as a generator or plant bus in PSS/E must be specified in a generator data record. In particular, each bus specified in the bus data input with a type code of two (2) or three (3) must have a generator data record entered for it.\n\nFields\n\ni::Vector{Int64}\nBus number, or extended bus name enclosed in single quotes.\nid::Vector{WeakRefStrings.String3}\nOne- or two-character uppercase non blank alphanumeric machine identifier used to distinguish among multiple machines at bus \"I\". It is recommended that, at buses for which a single machine is present, the machine be designated as having the machine identifier ’1’. ID = ’1’ by default.\n\npg::Vector{Float64}\nGenerator active power output; entered in MW. PG = 0.0 by default.\nqg::Vector{Float64}\nGenerator reactive power output; entered in Mvar. QG needs to be entered only if the case, as read in, is to be treated as a solved case. QG = 0.0 by default.\n\nqt::Vector{Float64}\nMaximum generator reactive power output; entered in Mvar. For fixed output gen- erators (i.e., nonregulating), QT must be equal to the fixed Mvar output. QT = 9999.0 by default.\n\nqb::Vector{Float64}\nMinimum generator reactive power output; entered in Mvar. For fixed output generators, QB must be equal to the fixed Mvar output. QB = -9999.0 by default.\n\nvs::Vector{Float64}\nRegulated voltage setpoint; entered in pu. VS = 1.0 by default.\nireg::Vector{Int64}\nBus number, or extended bus name enclosed in single quotes, of a remote type 1 or 2 bus whose voltage is to be regulated by this plant to the value specified by VS. If bus IREG is other than a type 1 or 2 bus, bus \"I\" regulates its own voltage to the value specified by VS. IREG is entered as zero if the plant is to regulate its own voltage and must be zero for a type three (swing) bus. IREG = 0 by default.\n\nmbase::Vector{Float64}\nTotal MVA base of the units represented by this machine; entered in MVA. This quantity is not needed in normal power flow and equivalent construction work, but is required for switching studies, fault analysis, and dynamic simulation. MBASE = system base MVA by default.\n\nzr::Vector{Float64}\nComplex machine impedance, ZSORCE; entered in pu on MBASE base. This data is not needed in normal power flow and equivalent construction work, but is required for switching studies, fault analysis, and dynamic simulation. For dynamic simulation, this impedance must be set equal to the unsaturated subtransient impedance for those generators to be modeled by subtransient level machine models, and to unsaturated transient impedance for those to be modeled by classical or transient level models. For short-circuit studies, the saturated subtransient or transient impedance should be used. ZR = 0.0 by default.\n\nzx::Vector{Float64}\nSee zr. ZX = 1.0 by default.\nrt::Vector{Float64}\nStep-up transformer impedance, XTRAN; entered in pu on MBASE base. XTRAN should be entered as zero if the step-up transformer is explicitly modeled as a network branch and bus \"I\" is the terminal bus. RT+jXT = 0.0 by default.\n\nxt::Vector{Float64}\nSee rt. RT+jXT = 0.0 by default.\ngtap::Vector{Float64}\nStep-up transformer off-nominal turns ratio; entered in pu. GTAP is used only if XTRAN is nonzero. GTAP = 1.0 by default.\n\nstat::Vector{Bool}\nInitial machine status of one for in-service and zero for out-of-service. STAT = 1 by default.\n\nrmpct::Vector{Float64}\nPercent of the total Mvar required to hold the voltage at the bus controlled by this bus \"I\" that are to be contributed by the generation at bus \"I\"; RMPCT must be positive. RMPCT is needed if IREG specifies a valid remote bus and there is more than one local or remote voltage controlling device (plant, switched shunt, FACTS device shunt element, or VSC dc line converter) controlling the voltage at bus IREG to a setpoint. RMPCT is needed also if bus \"I\" itself is being controlled locally or remotely by one or more other setpoint mode voltage controlling devices. RMPCT = 100.0 by default.\n\npt::Vector{Float64}\nMaximum generator active power output; entered in MW. PT = 9999.0 by default.\npb::Vector{Float64}\nMinimum generator active power output; entered in MW. PB = -9999.0 by default.\noi::Vector{Int64}\nOwner number; (1 through the maximum number of owners at the current size level). Each machine may have up to four owners. By default, O1 is the owner to which bus \"I\" is assigned and O2, O3, and O4 are zero.\n\nfi::Vector{Float64}\nFraction of total ownership assigned to owner Oi; each Fi must be positive. The Fi values are normalized such that they sum to 1.0 before they are placed in the working case. By default, each Fi is 1.0.\n\n\n\n\n\n","category":"type"},{"location":"#PowerFlowData.Branches","page":"Home","title":"PowerFlowData.Branches","text":"struct Branches <: PowerFlowData.Records\n\nIn PSS/E, the basic transmission line model is an Equivalent Pi connected between network buses.\n\nData for shunt equipment units, such as reactors, which are connected to and switched with the line, are entered in the same data record.\n\nnote: Shunts connected to buses\nTo represent shunts connected to buses, that shunt data should be entered in the bus data record.\n\nnote: Transformers\nBranches to be modeled as transformers are not specified in this data category; rather, they are specified in the transformer data category.\n\nFields\n\nI::Vector{Int64}\nBranch \"from bus\" number, or extended bus name enclosed in single quotes.\nJ::Vector{Int64}\nBranch \"to bus\" number, or extended bus name enclosed in single quotes. \"J\" is entered as a negative number, or with a minus sign before the first character of the extended bus name, to designate it as the metered end; otherwise, bus \"I\" is assumed to be the metered end.\n\nCKT::Vector{String}\nOne- or two-character uppercase nonblank alphanumeric branch circuit identifier; the first character of CKT must not be an ampersand \"&\". It is recommended that single circuit branches be designated as having the circuit identifier '1'. CKT = '1' by default.\n\nR::Vector{Float64}\nBranch resistance; entered in pu. A value of R must be entered for each branch.\nX::Vector{Float64}\nBranch reactance; entered in pu. A nonzero value of X must be entered for each branch.\nB::Vector{Float64}\nTotal branch charging susceptance; entered in pu. B = 0.0 by default.\nRATEA::Vector{Int64}\nFirst loading rating; entered in MVA. If RATEA is set to 0.0, the default value, this branch will not be included in any examination of circuit loading.\nRatings are entered as: MVA_rated = sqrt(3)  E_base  I_rated  10^-6 where:\nE_base is the base line-to-line voltage in volts of the buses to which the terminal of the branch is connected.\nI_rated is the branch rated phase current in amperes.\n\nRATEB::Vector{Int64}\nSecond loading rating; entered in MVA. RATEB = 0.0 by default.\nRATEC::Vector{Int64}\nThird loading rating; entered in MVA. RATEC = 0.0 by default.\nGI::Vector{Float64}\nComplex admittance of the line shunt at the bus \"I\" end of the branch; entered in pu. BI is negative for a line connected reactor and positive for line connected capacitor. GI + jBI = 0.0 by default.\n\nBI::Vector{Float64}\nComplex admittance of the line shunt at the bus \"I\" end of the branch; entered in pu. BI is negative for a line connected reactor and positive for line connected capacitor. GI + jBI = 0.0 by default.\n\nGJ::Vector{Float64}\nComplex admittance of the line shunt at the bus \"J\" end of the branch; entered in pu. BJ is negative for a line connected reactor and positive for line connected capacitor. GJ + jBJ = 0.0 by default.\n\nBJ::Vector{Float64}\nComplex admittance of the line shunt at the bus \"J\" end of the branch; entered in pu. BJ is negative for a line connected reactor and positive for line connected capacitor. GJ + jBJ = 0.0 by default.\n\nST::Vector{Int64}\nInitial branch status where 1 designates in-service and 0 designates out-of-service. ST = 1 by default.\n\nLEN::Vector{Float64}\nLine length; entered in user-selected units. LEN = 0.0 by default.\nOi::Vector{Int64}\nOwner number; 1 through the maximum number of owners at the current size level. Each branch may have up to four owners. By default, O1 is the owner to which bus \"I\" is assigned and O2, O3, and O4 are zero.\n\nFi::Vector{Float64}\nFraction of total ownership assigned to owner Oi; each Fi must be positive. The Fi values are normalized such that they sum to 1.0 before they are placed in the working case. By default, each Fi is 1.0.\n\n\n\n\n\n","category":"type"},{"location":"#Alternatives","page":"Home","title":"Alternatives","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"In Julia, I am are aware of two other open-source packages with functionality to parse PSS/E files:","category":"page"},{"location":"","page":"Home","title":"Home","text":"PowerModels.jl\nPowerSystems.jl","category":"page"},{"location":"","page":"Home","title":"Home","text":"I have not used either so cannot recommend one over the other. From what I can see, these parsers are almost identical to each other. It seems PowerSystems.jl originally vendored the PowerModels.jl code, but the parsers may have diverged slightly over time.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Importantly, these alternatives take a completely different approach to this package. These other parsers read the .raw files as a String (e.g. using readlines), then operate on string data, and parse strings into other Julia types as necessary.","category":"page"},{"location":"","page":"Home","title":"Home","text":"PowerFlowData.jl (this package) reads the .raw files as a bytes buffer (Vector{UInt8}), then parses the bytes directly into Julia types.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Hopefully this will be much faster and more memory efficient, but benchmarks pending. For now, this package is being developed as a fun exercise. Feel encouraged to give this package a try (and open issues!), but these alternative parsers are surely more battle-tested!","category":"page"},{"location":"#Implementation-details","page":"Home","title":"Implementation details","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"We use Parsers.jl to parse bytes into Julia types. Broadly speaking, we use Parsers.Options to configure the parsing based on the .raw format (e.g. , characters are delimiters), and then Parsers.xparse to actually parse the bytes between delimiters into the expected Julia types. The expected Julia type depends on the category of data we are reading at that point in the file (buses, loads, …); if the PSS/E user manual says \"load records\" should come after \"bus records\", and each load record should have 12 columns with the first column containing an integer \"bus number\", then we try to parse the first value in a load record as an Int, and so on.","category":"page"}]
}
