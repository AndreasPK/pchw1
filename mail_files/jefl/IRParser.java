import java.util.*;

public class IRParser {
	public static void main(String[] args) {
				
		Scanner ir = new Scanner(System.in);
		String w;

		Program prog = new Program();
		prog.symbols = new HashMap<String, Entry>();
		boolean parseSymbols = true;

		Scanner line = new Scanner(ir.nextLine());
		prog.name = line.next();

		/* parse symbol table */

		while (ir.hasNextLine() && parseSymbols) {
			line = new Scanner(ir.nextLine());
			if ((w = line.next()).equals("STMTLIST")) {
				parseSymbols = false;
				}
			else {
				String w2 = line.next();
				if (line.hasNext()) {   
					ArrayEntry e = new ArrayEntry();
					e.id = w;
					switch (w2) {
						case "INT": e.dataType = DataType.INT; 
						break;
						case "FLOAT": e.dataType = DataType.FLOAT;
						break; 
						}
					e.rank = Integer.parseInt(line.next());
					e.lb = new int[e.rank];
					e.ub = new int[e.rank];
					for (int i = 0; i<2; i++) {
						e.lb[i] = Integer.parseInt(line.next());
						e.ub[i] = Integer.parseInt(line.next());
						}
					prog.symbols.put(w,e);
					}
				else {
					Entry e = new Entry();
					e.id = w;
					switch (w2) {
						case "INT": e.dataType = DataType.INT; 
						break;
						case "FLOAT": e.dataType = DataType.FLOAT;
						break; 
						}
					prog.symbols.put(w,e);
					}
				}
			}

		/* print symbol table */

        for (String s: prog.symbols.keySet()) {
			Entry e = prog.symbols.get(s);
            System.out.print(e.id);
			DataType dt = e.dataType;
			switch (dt) {
				case INT: System.out.print(" int");
				break;
				case FLOAT: System.out.print(" float");
				break; 
				}
			if (e instanceof ArrayEntry) {
				System.out.println(" "+((ArrayEntry)e).rank);
				}
			else 
				System.out.println();
			}

		/* parse statements */
		  
		// == STMTLIST
		prog.stmts = getStmtList(ir,prog.symbols);

		/* print statements */

        for (Statement stmt: prog.stmts) {
			if (stmt instanceof AssignStmt) {
				System.out.println("assignment statement:");
				System.out.println(((AssignStmt)stmt).var.entry.id + " // lhs: variable id");
				Expression expr = ((AssignStmt)stmt).expr;
				if (expr instanceof IntNum) {
					System.out.println(((IntNum)expr).value + " // rhs: int number");
					}
				else if (expr instanceof FloatNum) {
					System.out.println(((FloatNum)expr).value + " // rhs: float number");
					}
				}
			}
		}

	static ArrayList<Statement> getStmtList(Scanner ir, HashMap<String, Entry> symbols) {
		// == STMTLIST
		String w;
		ArrayList<Statement> list = new ArrayList<Statement>();
		
		while (!(w = new Scanner(ir.nextLine()).next()).equals("/STMTLIST")) {
				// w == ASSIGN || IF || FOR
				switch (w) {
	 			case "ASSIGN":
					list.add(getAssignStmt(ir,symbols));
				break;
				case "IF":
					// ... FILL IN ...
				break;
				case "FOR":
					// ... FILL IN ...
				break;
				}
			}
		return list;
		}

	static AssignStmt getAssignStmt(Scanner ir, HashMap<String, Entry> symbols) {
		// == ASSIGN 
		AssignStmt stmt = new AssignStmt();
		ir.nextLine(); // skip VAR
		stmt.var = getVarRef(symbols,ir);
		// == EXPR
		stmt.expr= getExpr(symbols,ir);
		return stmt;
		}

	static VarRef getVarRef(HashMap<String, Entry> symbols, Scanner ir) {
		// = VAR
		VarRef var = new VarRef();
		String w;
		Scanner line = new Scanner(ir.nextLine());
		line.next(); // skip ENTRY
		var.entry=symbols.get(line.next());;
		if ((w = new Scanner(ir.nextLine()).next()).equals("EXPRLIST")) {
			// ... FILL IN ...
			}
		return var;
		}

	static Expression getExpr(HashMap<String, Entry> symbols, Scanner ir) {
		// == EXPR
		String w;
		Expression expr=null;
		Scanner line = new Scanner(ir.nextLine());
		switch ((w = line.next())) {
			case "INT": 
				IntNum i = new IntNum();
				i.value = Integer.parseInt(line.next());
				expr = i;
			break;
			case "FLOAT": 
				FloatNum f = new FloatNum();
				f.value = Float.parseFloat(line.next());
				expr = f;
			break;
			// ... FILL IN ...
			}
		return expr;
		}

	}
