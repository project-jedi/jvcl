object Form1: TForm1
  Left = 357
  Top = 162
  Width = 410
  Height = 295
  Caption = 'JvHLEdPropDlg Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object RAHLEditor1: TJvHLEditor
    Left = 16
    Top = 16
    Width = 249
    Height = 233
    Cursor = crIBeam
    Lines.Strings = (
      'unit fMain;'
      ''
      'interface'
      ''
      'uses'
      
        '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Form' +
        's, Dialogs,'
      
        '  JvHLEditorPropertyForm, StdCtrls, Buttons, JvButtons, JvEditor' +
        ', RAHLEditor;'
      ''
      'type'
      '  TForm1 = class(TForm)'
      '    RAHLEditor1: TJvHLEditor;'
      '    RAhtButton1: TJvHTButton;'
      '    RAHLEdPropDlg1: TJvHLEdPropDlg;'
      '    procedure RAhtButton1Click(Sender: TObject);'
      '  private'
      '    { Private declarations }'
      '  public'
      '    { Public declarations }'
      '  end;'
      ''
      'var'
      '  Form1: TForm1;'
      ''
      'implementation'
      ''
      '{$R *.DFM}'
      ''
      'procedure TForm1.RAhtButton1Click(Sender: TObject);'
      'begin'
      '  RAHLEdPropDlg1.Execute;'
      'end;'
      ''
      'end.')
    GutterWidth = 0
    RightMarginColor = clSilver
    Completion.ItemHeight = 13
    Completion.Interval = 800
    Completion.ListBoxStyle = lbStandard
    Completion.CaretChar = '|'
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    SelForeColor = clHighlightText
    SelBackColor = clHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabStop = True
    UseDockManager = False
    Colors.Comment.Style = [fsItalic]
    Colors.Comment.ForeColor = clOlive
    Colors.Number.ForeColor = clNavy
    Colors.Strings.ForeColor = clMaroon
    Colors.Symbol.ForeColor = clBlue
    Colors.Reserved.Style = [fsBold]
    Colors.Preproc.ForeColor = clGreen
    Colors.Statement.Style = [fsBold]
  end
  object RAhtButton1: TJvHTButton
    Left = 280
    Top = 16
    Width = 85
    Height = 25
    Caption = 'Properties...'
    TabOrder = 1
    OnClick = RAhtButton1Click
  end
  object JvHLEdPropDlg1: TJvHLEdPropDlg
    JvHLEditor = RAHLEditor1
    ColorSamples.Strings = (
      '[Default]'
      'Plain text'
      'Selected text'
      ''
      '[Pascal]'
      '{ Syntax highlighting }'
      'procedure TMain.JvHLEditorPreviewChangeStatus(Sender: TObject);'
      'const'
      '  Modi: array[boolean] of string[10] = ('#39#39', '#39'Modified'#39');'
      '  Modes: array[boolean] of string[10] = ('#39'Overwrite'#39', '#39'Insert'#39');'
      'begin'
      '  with StatusBar, JvHLEditorPreview do'
      '  begin'
      '    Panels[0].Text := IntToStr(CaretY) + '#39':'#39' + IntToStr(CaretX);'
      '    Panels[1].Text := Modi[Modified];'
      '    if ReadOnly then'
      '      Panels[2].Text := '#39'ReadOnly'#39'    else if Recording then'
      '      Panels[2].Text := '#39'Recording'#39'    else'
      '      Panels[2].Text := Modes[InsertMode];'
      '    miFileSave.Enabled := Modified;'
      '  end;'
      'end;'
      '[]'
      ''
      '[CBuilder]'
      '/* Syntax highlighting */'
      '#include "zlib.h"'
      ''
      '#define local static'
      ''
      'local int crc_table_empty = 1;'
      ''
      'local void make_crc_table()'
      '{'
      '  uLong c;'
      '  int n, k;'
      '  uLong poly;            /* polynomial exclusive-or pattern */'
      '  /* terms of polynomial defining this crc (except x^32): */'
      '  static Byte p[] = {0,1,2,4,5,7,8,10,11,12,16,22,23,26};'
      ''
      '  /* make exclusive-or pattern from polynomial (0xedb88320L) */'
      '  poly = 0L;'
      '  for (n = 0; n < sizeof(p)/sizeof(Byte); n++)'
      '    poly |= 1L << (31 - p[n]);'
      ''
      '  for (n = 0; n < 256; n++)'
      '  {'
      '    c = (uLong)n;'
      '    for (k = 0; k < 8; k++)'
      '      c = c & 1 ? poly ^ (c >> 1) : c >> 1;'
      '    crc_table[n] = c;'
      '  }'
      '  crc_table_empty = 0;'
      '}'
      '[]'
      ''
      '[VB]'
      'Rem Syntax highlighting'
      'Sub Main()'
      '  Dim S as String'
      '  If S = "" Then'
      '   '#39' Do something'
      '   MsgBox "Hallo World"'
      '  End If'
      'End Sub'
      '[]'
      ''
      '[Sql]'
      '/* Syntax highlighting */'
      'declare external function Copy'
      '  cstring(255), integer, integer'
      '  returns cstring(255)'
      '  entry_point "Copy" module_name "nbsdblib";'
      '[]'
      ''
      '[Python]'
      '# Syntax highlighting'
      ''
      'from Tkinter import *'
      'from Tkinter import _cnfmerge'
      ''
      'class Dialog(Widget):'
      '  def __init__(self, master=None, cnf={}, **kw):'
      '    cnf = _cnfmerge((cnf, kw))'
      
        '    self.widgetName = '#39'__dialog__'#39'    Widget._setup(self, master' +
        ', cnf)'
      '    self.num = self.tk.getint('
      '      apply(self.tk.call,'
      '            ('#39'tk_dialog'#39', self._w,'
      '             cnf['#39'title'#39'], cnf['#39'text'#39'],'
      '             cnf['#39'bitmap'#39'], cnf['#39'default'#39'])'
      '            + cnf['#39'strings'#39']))'
      '    try: Widget.destroy(self)'
      '    except TclError: pass'
      '  def destroy(self): pass'
      '[]'
      ''
      '[Java]'
      '/* Syntax highlighting */'
      'public class utils {'
      
        '  public static String GetPropsFromTag(String str, String props)' +
        ' {'
      '    int bi;'
      '    String Res = "";'
      '    bi = str.indexOf(props);'
      '    if (bi > -1) {'
      '      str = str.substring(bi);'
      '      bi  = str.indexOf("\"");'
      '      if (bi > -1) {'
      '        str = str.substring(bi+1);'
      '        Res = str.substring(0, str.indexOf("\""));'
      '      } else Res = "true";'
      '    }'
      '    return Res;'
      '  }'
      '[]'
      ''
      '[Html]'
      '<html>'
      '<head>'
      '<meta name="GENERATOR" content="Microsoft FrontPage 3.0">'
      '<title>JVCLmp;A Library home page</title>'
      '</head>'
      ''
      
        '<body background="zertxtr.gif" bgcolor="#000000" text="#FFFFFF" ' +
        'link="#FF0000"'
      'alink="#FFFF00">'
      ''
      
        '<p align="left">Download last JVCLmp;A Library version now - <fo' +
        'nt face="Arial"'
      
        'color="#00FFFF"><a href="http://www.torry.ru/vcl/packs/ralib.zip' +
        '"><small>ralib110.zip</small></a>'
      
        '</font><font face="Arial" color="#008080"><small><small>(575 Kb)' +
        '</small></small></font>.</p>'
      ''
      '</body>'
      '</html>'
      '[]'
      ''
      '[Perl]'
      '#!/usr/bin/perl'
      '# Syntax highlighting'
      ''
      'require "webtester.pl";'
      ''
      '$InFile = "/usr/foo/scripts/index.shtml";'
      '$OutFile = "/usr/foo/scripts/sitecheck.html";'
      '$MapFile = "/usr/foo/scripts/sitemap.html";'
      ''
      'sub MainProg {'
      #9'require "find.pl";'
      #9'&Initialize;'
      #9'&SiteCheck;'
      #9'if ($MapFile) { &SiteMap; }'
      #9'exit;'
      '}'
      '[Ini]'
      ' ; Syntax highlighting'
      ' [drivers]'
      ' wave=mmdrv.dll'
      ' timer=timer.drv'
      ''
      ' plain text'
      '[Coco/R]'
      'TOKENS'
      '  NUMBER = digit { digit } .'
      '  EOL = eol .'
      ''
      'PRODUCTIONS'
      ''
      'ExprPostfix   ='
      '                       (. Output := '#39#39'; .)'
      '      Expression<Output>  EOL'
      '                       (. ShowOutput(Output); .)'
      '    .'
      '[]')
    Pages = [epEditor, epColors]
    OnDialogPopup = RAHLEdPropDlg1DialogPopup
    OnDialogClosed = RAHLEdPropDlg1DialogClosed
    Left = 312
    Top = 104
  end
end
