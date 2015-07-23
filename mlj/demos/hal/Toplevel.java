/*
 * WebTerm version 1.3
 * ~~~~~~~
 *
 * Terminal class: a low-level terminal screen driver, encapsulated as
 * a widget.
 *
 * Written by Dianne Hackborn and Melanie Johnson.
 *
 ********************************************************************************************
 * Crudely hacked by Nick to turn it into a simple front-end for green-screen SML           *
 * applications which we wish to deliver as applets.                                        *
 ********************************************************************************************
 *
 * The design and implementation of WebTerm are available for
 * royalty-free adoption and use for non-commercial purposes, by any
 * public or private organization.  Copyright is retained by the
 * Northwest Alliance for Computational Science and Engineering and
 * Oregon State University.  Redistribution of any part of WebTerm or
 * any derivative works must include this notice.
 *
 * All Rights Reserved.
 *
 * Please address correspondence to nacse-questions@nacse.org.
 *
 * ----------------------------------------------------------------------
 *
 * Known Bugs
 * ~~~~~~~~~~
 *
 * - The page buffer will occasionally get null pointers placed into it,
 *   causing an error report to the console.  This doesn't cause any
 *   long-term harm to the applet's execution, however.
 *
 * ----------------------------------------------------------------------
 *
 * History
 * ~~~~~~~
 *
 * 1.3: Created this file.
 *      Added REGISTRY parameter.
 *      Cleaned up font allocation in init(), fixed bugs.
 *      Added FONTSIZE parameter.
 *      preferredSize() now just returns minimumSize().
 *
 */

import java.applet.Applet;    
import java.awt.*; 
import java.io.*;

public class Toplevel extends Applet implements SchanReader {
 Terminal myterminal;


  public void init() {
   myterminal = new Terminal();
  
   HalServer.registerReader(this); /* Tell the ML code where to put the output */

   resize(800,400);
  GridBagLayout gb = new GridBagLayout();
  setLayout(gb);
  GridBagConstraints cn = new GridBagConstraints();
  cn.weightx = 1;
  cn.weighty = 1;
  cn.fill = cn.BOTH;
  gb.setConstraints(myterminal,cn);
  add(myterminal);
  validate();
  show();
  myterminal.init();
  myterminal.write("Hal Applet Compiled by MLj 0.2\n",0);
  }


  // This is the method by which we implement the SchanReader interface
  public void handlechan (String thedata) {
    myterminal.write(thedata, 0);
  }

  // when the applet gets asked to stop we should stop 
  // any background computation
  public void stop() {
      if (Terminal.computeThread != null && Terminal.computeThread.isAlive()) {
      Terminal.computeThread.stop();
      myterminal.write("**background computation stopped**\n",0);
	}
      Terminal.computeThread = null;

  }


  // not sure if this helps...
  public void start() {
   Terminal.computeThread = null;
  }
  
}


class Terminal extends Canvas {
  // this holds the thread which is currently doing some ML computation
  // or null if there isn't one.
  public static Thread computeThread = null;

  static final boolean DEBUG = false;

  static final int OUTF_RAW     = 1<<0;
  static final int OUTF_PARTIAL = 1<<1;

  static final int STYLE_PLAIN      = 0;
  static final int STYLE_INVERSE    = 1<<0;
  static final int STYLE_BOLD       = 1<<1;
  static final int STYLE_ITALIC     = 1<<2;
  static final int STYLE_UNDERSCORE = 1<<3;

  static final int INNER_SIZE = 4;
  static final int ACTIVE_SIZE = 2;

  static final int BORDER_WIDTH = (INNER_SIZE+ACTIVE_SIZE);
  static final int BORDER_HEIGHT = (INNER_SIZE+ACTIVE_SIZE);

  int fontsize = -1;

  Color defBackground, defForeground;
  Toolkit toolkit;

  Font fontPlain, fontBold, fontItalic, fontBoldItalic;
  int charWidth, charHeight;
  int fontAscent, fontDescent;

  int numRows, numCols;
  int curRow, curCol;
  int curStyle = STYLE_PLAIN;

  int regionTop, regionBottom, regionLeft, regionRight;

  int lastRow, lastCol;
  int dirtyTop = -1, dirtyBottom = -1;
  int scrollCount = 0;
  boolean overChar = false;
  boolean cursorVisible = true;
  char page[][];
  int style[][];

// initialize
  public Terminal() {
   fontsize = 12;
   
  }

    public void init() {
    Graphics g = getGraphics();
    Font f = g.getFont();
    int size = f.getSize();
    if( fontsize > 0 ) size = fontsize;
    fontPlain = f;
    try {
      fontPlain = new Font("Courier",Font.PLAIN,size);
    }
    finally {}
    if( fontPlain == null ) fontPlain = f;
    if( DEBUG ) {
      System.out.println("StdFont name=" + fontPlain.getName()
			 + " size=" + fontPlain.getSize()
			 + " style=" + fontPlain.getStyle());
    }
    fontBold = fontPlain;
    try {
      fontBold = new Font(fontPlain.getName(),Font.BOLD,
			  fontPlain.getSize());
    }
    finally {}
    fontItalic = fontBoldItalic = fontBold;
    try {
      fontItalic = new Font(fontPlain.getName(),Font.ITALIC,
			    fontPlain.getSize());
    }
    finally {}
    try {
      fontBoldItalic = new Font(fontPlain.getName(),Font.BOLD+Font.ITALIC,
				fontPlain.getSize());
    }
    finally {}
    if( DEBUG ) {
      System.out.println("BldFont name=" + fontBold.getName()
			 + " size=" + fontBold.getSize()
			 + " style=" + fontBold.getStyle());
      System.out.println("ItlFont name=" + fontItalic.getName()
			 + " size=" + fontItalic.getSize()
			 + " style=" + fontItalic.getStyle());
    }
    g.setFont(fontPlain);
    setFont(fontPlain);
    defBackground = getBackground();
    defForeground = getForeground();
    toolkit = getToolkit();

    screenInit();
    repaint();
  }

  FontMetrics get_metrics(boolean install) {
    Graphics g;
    FontMetrics m;
    Dimension d;

    g = getGraphics();
    m = g.getFontMetrics(fontPlain);
    if( install ) {
      charWidth = m.charWidth('W');
      charHeight = m.getHeight();
      fontAscent = m.getAscent();
      fontDescent = m.getDescent();
    }

    return m;
  }

  public Dimension minimumSize() {
    return (new Dimension(800,400));
    /*
    FontMetrics m = get_metrics(false);
    Dimension dim = new Dimension((m.charWidth('W')*4) + BORDER_WIDTH*2,
				  (m.getHeight()*2) + BORDER_HEIGHT*2);
    if( DEBUG ) System.out.println("Minimum dim: " + dim);
    return dim;
    */
  }

  public Dimension preferredSize() {
    // When the requested size is smaller than both preferred dimensions,
    // the layout manager seems to just use the preferred size, rather
    // than falling back to the minimum size.  Yuck.
    return minimumSize();
    /*
    FontMetrics m = get_metrics(false);
    Dimension dim = new Dimension((m.charWidth('W')*80) + BORDER_WIDTH*2,
				  (m.getHeight()*24) + BORDER_HEIGHT*2);
    if (DEBUG) System.out.println("Preferred dim: " + dim);
    return dim;
    */
  }

  public void screenInit() {
    Dimension d;
    int rows,cols;

    get_metrics(true);

    if( DEBUG ) System.out.println("Initializing screen...");

    d = size();
    if (DEBUG) System.out.println("size: " + d +" bh: " + BORDER_HEIGHT + "bw: " + BORDER_WIDTH + "cw:" + charWidth + " ch: " + charHeight);

    cols = (d.width-(BORDER_WIDTH*2)) / charWidth;
    rows = (d.height-(BORDER_HEIGHT*2)) / charHeight;
    if( cols < 4 ) cols = 4;
    if( rows < 2 ) rows = 2;

    if( numRows == rows && numCols == cols ) return;

    numRows = rows;
    numCols = cols;
    curCol = 0;
    curRow = 0;
    lastCol = 0;
    lastRow = 0;
    scrollCount = 0;
    setRegion();

    if( DEBUG ) System.out.println("Width = " + d.width +
				   ", Height = " + d.height);
    if( DEBUG ) System.out.println("Columns = " + numCols +
				   ", Rows = " + numRows);
    page = new char[numRows][numCols];
    style = new int[numRows][numCols];

    for( int r = 0; r < numRows; r++ ) {
      for( int c = 0; c < numCols; c++ ) {
	page[r][c] = ' ';
	style[r][c] = STYLE_PLAIN;
      }
    }

    dirtyTop = 0;
    dirtyBottom = numRows-1;

  }

  /* shouldn't need this since the terminal has no subcomponents 
  public void layout() {
    if( DEBUG ) System.out.println("Doing layout of terminal.");
    screenInit();
  }
  */

  public int getCursorRow() {
    return curRow;
  }

  public int getCursorCol() {
    return curCol;
  }

  public int getNumRows() {
    return numRows;
  }

  public int getNumCols() {
    return numCols;
  }

  public void setCursorPos(int row, int col) {
    if( row < 0 ) row = 0;
    if( row >= numRows ) row = numRows-1;
    if( col < 0 ) col = 0;
    if( col >= numCols ) col = numCols-1;
    curRow = row;
    curCol = col;
  }

  public void setCursorVisible(boolean state) {
    cursorVisible = state;
  }

  public boolean getCursorVisible() {
    return cursorVisible;
  }

  public void setRegion() {
    regionTop = 0;
    regionBottom = numRows-1;
    regionLeft = 0;
    regionRight = numCols-1;
  }

  public void setRegion(int top, int bottom) {
    setRegion(top,bottom,0,numCols-1);
  }

  public void setRegion(int top, int bottom, int left, int right) {
    if( top < 0 ) top = 0;
    if( top >= numRows ) top = numRows-1;
    if( bottom < 0 ) bottom = 0;
    if( bottom >= numRows ) bottom = numRows-1;
    if( top >= bottom-2 ) {
      if( top > 2 ) top = 0;
      else bottom = numRows-1;
    }
    if( left < 0 ) left = 0;
    if( left >= numCols ) left = numCols-1;
    if( right < 0 ) right = 0;
    if( right >= numCols ) right = numCols-1;
    if( left >= right-2 ) {
      if( left > 2 ) left = 0;
      else right = numCols-1;
    }
    regionTop = top;
    regionBottom = bottom;
    regionLeft = left;
    regionRight = right;
  }

  public int getRegionTop() {
    return regionTop;
  }

  public int getRegionBottom() {
    return regionBottom;
  }

  public int getRegionLeft() {
    return regionLeft;
  }

  public int getRegionRight() {
    return regionRight;
  }

  public void setStyle(int style) {
    curStyle = style;
  }

  public int getStyle() {
    return curStyle;
  }

  public void setChar(char c) {
    page[curRow][curCol] = c;
    style[curRow][curCol] = curStyle;
  }

  public void setChar(char c, int row, int col) {
    page[row][col] = c;
    style[row][col] = curStyle;
  }

  public char getChar() {
    return page[curRow][curCol];
  }

  public char getChar(int row, int col) {
    return page[row][col];
  }

  public int getCharStyle() {
    return style[curRow][curCol];
  }

  public int getCharStyle(int row, int col) {
    return style[row][col];
  }

  Font get_style_font(int style) {
    switch( style&(STYLE_BOLD|STYLE_ITALIC) ) {
    case STYLE_PLAIN:
      return fontPlain;
    case STYLE_BOLD:
      return fontBold;
    case STYLE_ITALIC:
      return fontItalic;
    case STYLE_BOLD|STYLE_ITALIC:
      return fontBoldItalic;
    default:
      return fontPlain;
    }
  }

  void drawRowCol(Graphics g, int row, int col) {
    int x = ( col * charWidth ) + BORDER_WIDTH;
    int y = ( row * charHeight ) + BORDER_HEIGHT;
    g.setFont(get_style_font(style[row][col]));
    if( ( curRow != row || curCol != col || !cursorVisible )
       ^ ( (style[row][col]&STYLE_INVERSE) == STYLE_INVERSE) ) {
      g.clearRect(x,y,charWidth,charHeight);
      g.setColor(getForeground());
      g.drawChars(page[row],col,1,x,y+fontAscent);
    } else {
      g.setColor(getForeground());
      g.fillRect(x,y,charWidth,charHeight);
      g.setColor(getBackground());
      g.drawChars(page[row],col,1,x,y+fontAscent);
    }
    if( (style[row][col]&STYLE_UNDERSCORE) != 0 ) {
      g.drawLine(x,y+fontAscent,x+charWidth-1,y+fontAscent);
    }
  }

  public void drawRow(Graphics g, int row) {
    int[] rowStyles = style[row];
    int col=0;
    int y = BORDER_HEIGHT+(row*charHeight);

    if( rowStyles == null || page[row] == null ) {
      System.out.println("*** null entry at row " + row);
      return;
    }

    while( col < numCols ) {
      int first=col;
      int cur_style = rowStyles[col];
      while( col < numCols && cur_style == rowStyles[col] ) {
	col++;
      }
      int x = BORDER_WIDTH+(first*charWidth);
      g.setFont(get_style_font(cur_style));
      if( (cur_style&STYLE_INVERSE) == 0 ) {
	g.setColor(getBackground());
      } else {
	g.setColor(getForeground());
      }
      g.fillRect(x,y,((col-first)*charWidth),charHeight);
      if( (cur_style&STYLE_INVERSE) == 0 ) {
	g.setColor(getForeground());
      } else {
	g.setColor(getBackground());
      }
      g.drawChars(page[row],first,col-first,x,y+fontAscent);
      if( (cur_style&STYLE_UNDERSCORE) != 0 ) {
	g.drawLine(x,y+fontAscent,x+((col-first)*charWidth),y+fontAscent);
      }
    }
    if( cursorVisible && row == curRow ) {
      if( DEBUG ) System.out.println("Drawing cursor: row=" + curRow +
				     ", col=" + curCol);
      drawRowCol(g,curRow,curCol);
    }
  }

  public void screenRedraw(Graphics g, int top, int bot) {
    if( top < 0 ) top = 0;
    if( bot >= numRows ) bot = numRows-1;
    g.setFont(fontPlain);
    if( DEBUG ) System.out.println("Redrawing: " + top + " to " + bot);
    for( int i = top; i <= bot; i++ ) {
      if( DEBUG ) System.out.println("Drawing: " + i + ", curRow: " +
				     curRow);
      drawRow(g,i);
/*
      g.setColor(getBackground());
      g.fillRect(BORDER_WIDTH,BORDER_HEIGHT+(i*charHeight),
		 (numCols*charWidth),
		 charHeight);
      g.setColor(getForeground());
      g.drawChars(page[i],0,numCols,BORDER_WIDTH,
		  BORDER_HEIGHT + (i*charHeight + fontAscent));
      if( cursorVisible && i == curRow ) {
	if( DEBUG ) System.out.println("Drawing cursor: row=" + curRow +
				       ", col=" + curCol);
	drawRowCol(g,curRow,curCol);
      }
*/
    }
    /* toolkit.sync(); */
    scrollCount = 0;
  }

  public void screenRedraw(Graphics g) {
    screenRedraw(g,0,numRows-1);
  }

  public void screenClean(Graphics g) {
    if( DEBUG ) System.out.println("Cleaning screen: " + dirtyTop +
				   " to " + dirtyBottom);
    if( dirtyTop >= 0 && dirtyBottom >= 0 ) {
      drawRowCol(g,lastRow,lastCol);
      screenRedraw(g,dirtyTop,dirtyBottom);
    }
    if( lastRow != curRow || lastCol != curCol ) {
      drawRowCol(g,lastRow,lastCol);
      drawRowCol(g,curRow,curCol);
    }
    dirtyTop = -1;
    dirtyBottom = -1;
    lastRow = curRow;
    lastCol = curCol;
    toolkit.sync(); 
  }

  void screenDirty(int top, int bottom) {
    if( dirtyTop < 0 || top < dirtyTop ) dirtyTop = top;
    if( bottom > dirtyBottom ) dirtyBottom = bottom;
  }

  public void screenScrollRegion(int top, int bottom, int amount) {
    if( top >= bottom ) return;
    if( amount < 0 ) {
      amount = -amount;
      for( int i = top; i < (bottom-amount+1); i++ ) {
	page[i] = page[i+amount];
	style[i] = style[i+amount];
      }
      for( int i = bottom-amount+1; i < bottom+1; i++ ) {
	page[i] = new char[numCols];
	style[i] = new int[numCols];
	for( int j = 0; j < numCols; j++ ) {
	  page[i][j] = ' ';
	  style[i][j] = STYLE_PLAIN;
	}
      }
      /*
      cursorVisible = false;
      drawRowCol(g,curRow,curCol);
      g.copyArea(0,amount*charHeight,
		 numCols*charWidth,(numRows-amount)*charHeight,
		 0,-amount*charHeight);
      if( dirtyTop >= 0 ) {
	dirtyTop-=amount;
	if( dirtyTop < 0 ) dirtyTop = 0;
      }
      if( dirtyBottom >= 0 ) {
	dirtyBottom-=amount;
	if( dirtyBottom < 0 ) dirtyBottom = 0;
      }
      screenDirty(numRows-amount,numRows-1);
      cursorVisible = true;
      drawRowCol(g,curRow,curCol);
      */
      screenDirty(top,bottom);
    } else if( amount > 0 ) {
      for( int i = bottom; i >= top+amount; i-- ) {
	page[i] = page[i-amount];
	style[i] = style[i-amount];
      }
      for( int i = top; i < top+amount; i++ ) {
	page[i] = new char[numCols];
	style[i] = new int[numCols];
	for( int j = 0; j < numCols; j++ ) {
	  page[i][j] = ' ';
	  style[i][j] = STYLE_PLAIN;
	}
      }
      /*
      cursorVisible = false;
      drawRowCol(g,curRow,curCol);
      g.copyArea(0,0,
		 numCols*charWidth,(numRows-amount)*charHeight,
		 0,amount*charHeight);
      if( dirtyTop >= 0 ) {
	dirtyTop+=amount;
	if( dirtyTop >= numRows ) dirtyTop = numRows-1;
      }
      if( dirtyBottom >= 0 ) {
	dirtyBottom+=amount;
	if( dirtyBottom >= numRows ) dirtyBottom = numRows-1;
      }
      screenDirty(0,amount-1);
      cursorVisible = true;
      drawRowCol(g,curRow,curCol);
      */
      screenDirty(top,bottom);
    }
    /* toolkit.sync(); */
  }

  public void screenScroll(int amount) {
    screenScrollRegion(regionTop,regionBottom,amount);
  }

  public void screenClearEOL() {
    for( int i=curCol; i<=regionRight; i++ ) {
      page[curRow][i] = ' ';
      style[curRow][i] = STYLE_PLAIN;
    }
    screenDirty(curRow,curRow);
  }

  public void screenClearBOL() {
    for( int i=regionLeft; i<=curCol; i++ ) {
      page[curRow][i] = ' ';
      style[curRow][i] = STYLE_PLAIN;
    }
    screenDirty(curRow,curRow);
  }

  public void screenClearLine() {
    for( int i=regionLeft; i<=regionRight; i++ ) {
      page[curRow][i] = ' ';
      style[curRow][i] = STYLE_PLAIN;
    }
    screenDirty(curRow,curRow);
  }

  public void screenClearEOD() {
    int left = curCol;
    for( int j=curRow; j<=regionBottom; j++ ) {
      for( int i=left; i<=regionRight; i++ ) {
	page[j][i] = ' ';
	style[j][i] = STYLE_PLAIN;
      }
      left = regionLeft;
    }
    screenDirty(curRow,regionBottom);
  }

  public void screenClearBOD() {
    for( int j=regionTop; j<curRow; j++ ) {
      for( int i=regionLeft; i<=regionRight; i++ ) {
	page[j][i] = ' ';
	style[j][i] = STYLE_PLAIN;
      }
    }
    for( int i=regionLeft; i<curCol; i++ ) {
      page[curRow][i] = ' ';
      style[curRow][i] = STYLE_PLAIN;
    }
    screenDirty(regionTop,curRow);
  }

  public void screenClear() {
    for( int j=regionTop; j<=regionBottom; j++ ) {
      for( int i=regionLeft; i<=regionRight; i++ ) {
	page[j][i] = ' ';
	style[j][i] = STYLE_PLAIN;
      }
    }
    screenDirty(regionTop,regionBottom);
  }

  public synchronized void write(char[] d, int off, int len, int flags) {
    if( DEBUG ) {
      if( d == null ) d = new char[0];
      System.out.println("Write: " + new String(d,off,len));
    }
    if( len == 1 ) {
      write(d[off],flags);
    } else if( len > 1 ) {
      while( len > 0 ) {
	int last_row = curRow;
	boolean do_redraw;
	if( (flags&OUTF_RAW) != 0 ) do_redraw = put_char(d[off]);
	else do_redraw = put_std_char(d[off]);
	if( do_redraw ) {
	  if( last_row < curRow ) screenDirty(last_row,curRow);
	  else screenDirty(curRow,last_row);
	}
	off++;
	len--;
      }
      if( (flags&OUTF_PARTIAL) == 0 ) screenClean(getGraphics());
    } else if( (flags&OUTF_PARTIAL) == 0 ) screenClean(getGraphics());
  }

  public void write(String str, int flags) {
    write(str.toCharArray(),0,str.length(),flags);
  }

  public synchronized void write(char c, int flags) {
    int last_col = curCol, last_row = curRow;
    boolean do_redraw;
    if( (flags&OUTF_RAW) != 0 ) do_redraw = put_char(c);
    else do_redraw = put_std_char(c);
    if( do_redraw ) {
      if( last_row < curRow ) screenDirty(last_row,curRow);
      else screenDirty(curRow,last_row);
    }
    if( (flags&OUTF_PARTIAL) == 0 ) screenClean(getGraphics());
  }

  boolean put_char(char c) {
    if( curCol >= regionRight && !overChar ) {
      page[curRow][curCol] = c;
      style[curRow][curCol] = curStyle;
      curCol = regionRight;
      overChar = true;
    } else if (curCol >= regionRight && overChar) {
      overChar = false;
      screenScroll(-1);
      curRow = regionBottom;
      curCol = 1;
      page[curRow][0] = c;
      style[curRow][0] = curStyle;
      screenDirty(regionBottom-1,regionBottom);
    } else {
      page[curRow][curCol] = c;
      style[curRow][curCol] = curStyle;
      curCol++;
      overChar = false;
    }
    return true;
  }

  static final char CTRL_A = (char)1;
  static final char CTRL_B = (char)2;
  static final char CTRL_C = (char)3;
  static final char CTRL_D = (char)4;
  static final char CTRL_E = (char)5;
  static final char CTRL_F = (char)6;
  static final char CTRL_G = (char)7;
  static final char CTRL_H = (char)8;
  static final char CTRL_I = (char)9;
  static final char CTRL_J = (char)10;
  static final char CTRL_K = (char)11;
  static final char CTRL_L = (char)12;
  static final char CTRL_M = (char)13;
  static final char CTRL_N = (char)14;
  static final char CTRL_O = (char)15;
  static final char CTRL_P = (char)16;
  static final char CTRL_Q = (char)17;
  static final char CTRL_R = (char)18;
  static final char CTRL_S = (char)19;
  static final char CTRL_T = (char)20;
  static final char CTRL_U = (char)21;
  static final char CTRL_V = (char)22;
  static final char CTRL_W = (char)23;
  static final char CTRL_X = (char)24;
  static final char CTRL_Y = (char)25;
  static final char CTRL_Z = (char)26;

  boolean put_std_char(char c) {
    boolean do_redraw = false;
    
    switch( c ) {
    case 0:
      break;
    case CTRL_G:     // Bell
      break;
    case CTRL_I:     // Tab
      break;
    case '\n':
      if( curRow >= regionBottom ) {
	screenScroll(-1);
	curRow = regionBottom;
	screenDirty(regionBottom,regionBottom);
      } else {
	curRow++;
      }
      overChar = false;
      curCol = regionLeft;
      break;
    case '\r':
      //curCol = regionLeft;
      //do_redraw = true;
      break;
    case '\f':
      screenInit();
      break;
    case CTRL_H: // Backspace
    case 0x7f:
    if( DEBUG ) System.out.println("Delete pressed:" + curCol + ","+regionRight);
    if (curCol>0) {
      for( int i=curCol-1; (i<regionRight); i++ ) {
        // if( DEBUG ) System.out.println("replacing " +i+":" + page[curRow][i] + " with " + page[curRow][i+1]);
	page[curRow][i] = page[curRow][i+1];
	style[curRow][i] = style[curRow][i+1];
      }
      page[curRow][regionRight] = ' ';
      style[curRow][regionRight] = STYLE_PLAIN;
      screenDirty(curRow,curRow);
      curCol=curCol-1;
      do_redraw = true;
	}
      else {
       do_redraw = false;
	 }
      break;
    default:
      do_redraw = put_char(c);
      break;
    }

    return do_redraw;
  }

//  
// handle input 
// 
  public synchronized boolean keyDown(Event e, int key ) {
 
    if( DEBUG ) System.out.println("Key event: " + key);

    if (computeThread != null) {
      // we've hit a key whilst still doing a computation. 
      // So stop that, print a message
      // and carry on.
      if(computeThread.isAlive()) {
        computeThread.stop();
        write("**interrupted**\n",0);
        computeThread = null;
        return true;
      }
      computeThread = null; //null it out anyway
    }

    // This is amazingly nasty code and only deals with single lines of input
    // and doesn't work with that rotten threading stuff right..
    if( key == '\n' ) {
      String s = new String();
      for (int i=0; i<curCol; i++) {
        s += page[curRow][i];
      }
      write((char)key,0); // send the return character to the window

      computeThread = HalServer.processline(s); 
      // now pass the string to HalServer, getting
      // back a thread which is executing it.

    } else {
      write((char)key,0);
    }
    return true;

  }

  public boolean mouseDown(Event evt, int x, int y) {
    requestFocus();
    return true;
  }

  boolean have_focus = false;

  void draw_border(Graphics g) {
    Dimension d = size();
    if( have_focus ) {
      g.setColor(getForeground());
    } else {
      g.setColor(getBackground());
    }
    g.drawRect(0,0,d.width,d.height);
    g.drawRect(1,1,d.width-2,d.height-2);
    g.setColor(Color.white);
    g.draw3DRect(2,2,d.width-4,d.height-4,true);
    g.draw3DRect(3,3,d.width-6,d.height-6,true);
  }

  public void paint(Graphics g) {
    draw_border(g);
    screenRedraw(g);
  }

  public boolean gotFocus(Event evt, Object what) {
    Graphics g = getGraphics();
    have_focus = true;
    if( g != null ) draw_border(g);
    return true;
  }

  public boolean lostFocus(Event evt, Object what) {
    Graphics g = getGraphics();
    have_focus = false;
    if( g != null ) draw_border(g);
    return true;
  }

  public final synchronized void update (Graphics g) {
    paint(g);
  }
}
