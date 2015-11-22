class X extends Exception {};

class XTest {
    static int f (int x) throws X {
	if (x > 8000)
	    throw new X();
	else 
	    return f (x+1);
    }
    
    static int g (int x) {
	if (x > 8000)
	    return 0;
	else
	    return g (x+1);
    }


    static int h (int x) throws X {
	if (x > 8000) 
	    throw new X();
	else 
	    try {
		return h (x+1);
	    } catch (X x0) { 
		throw x0;
	    }
    }

    static int k (int x) throws X {
	if (x > 8000) 
	    return 0;
	else 
	    try {
		return k (x+1);
	    } catch (X x0) { 
		throw x0;
	    }
    }

    public static void main (String argv[]) {
	char opt = argv[0].charAt(0);
	int n = Integer.parseInt (argv[1]);
	for (int i = 0; i < n; i++) {
         try {
	     switch (opt) {
	     case 'f': f(0); break;
	     case 'g': g(0); break;
	     case 'h': h(0); break;
	     case 'k': k(0); break;
	     }
	    } catch (X x) {
	    } 
	}
    }
}
		    
