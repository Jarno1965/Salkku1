BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "Stats" (
	"Portfolio"	TEXT,
	"Trikker"	TEXT,
	"Description"	TEXT,
	"Comment"	TEXT,
	"Value"	REAL
);
CREATE TABLE IF NOT EXISTS "Currency" (
	"date"	REAL,
	"rate"	REAL,
	"currency"	TEXT
);
CREATE TABLE IF NOT EXISTS "Portfolios" (
	"PortfolioName"	TEXT,
	"date"	REAL,
	"MoneyTransfer"	TEXT,
	"Tricker"	TEXT,
	"Qnt"	REAL,
	"Price"	REAL,
	"CloseVal"	REAL,
	"Total"	REAL
);
CREATE TABLE IF NOT EXISTS "Portfolio" (
	"Name"	TEXT,
	"Description"	TEXT,
	"LastUpdate"	TEXT
);
CREATE TABLE IF NOT EXISTS "Groups" (
	"Name"	TEXT,
	"Portfolio"	TEXT
);
CREATE TABLE IF NOT EXISTS "Trickers" (
	"Tricker"	TEXT,
	"Description"	TEXT,
	"FirstData"	TEXT,
	"LastUpdate"	TEXT,
	"OsingonValuutta"	TEXT,
	"KurssienValuutta"	TEXT
);
CREATE TABLE IF NOT EXISTS "HandlingData" (
	"GroupName"	TEXT,
	"DataType"	TEXT,
	"Value"	TEXT
);
COMMIT;
