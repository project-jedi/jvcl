object frmEditor: TfrmEditor
  Left = 321
  Top = 100
  Width = 549
  Height = 428
  Caption = 'TJvEditor and TJvHLEditor components demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 33
    Width = 541
    Height = 346
    ActivePage = TabSheet1
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    TabStop = False
    OnChange = PageControl1Change
    OnEnter = PageControl1Enter
    object TabSheet1: TTabSheet
      Caption = 'Delphi'
      object JvEditor: TJvHLEditor
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          'unit fJvEditorTest;'
          ''
          'interface'
          ''
          'uses'
          
            '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Form' +
            's, Dialogs,'
          '  JvEditor, IParser, StdCtrls, ExtCtrls, ComCtrls;'
          ''
          'type'
          '  TJvEditor  = class(TForm)'
          '    GutterImages: TImageList;'
          '    PageControl1: TPageControl;'
          '    TabSheet1: TTabSheet;'
          '    JvEditor: TJvEditor;'
          '    TabSheet2: TTabSheet;'
          '    RAEditor1: TJvEditor;'
          '    TabSheet3: TTabSheet;'
          '    RAEditor2: TJvEditor;'
          '    TabSheet4: TTabSheet;'
          '    TabSheet5: TTabSheet;'
          '    RAEditor3: TJvEditor;'
          '    RAEditor4: TJvEditor;'
          '    Panel1: TPanel;'
          '    Label1: TLabel;'
          '    Label2: TLabel;'
          '    ilCompletions: TImageList;'
          '    procedure FormCreate(Sender: TObject);'
          '    procedure FormDestroy(Sender: TObject);'
          
            '    procedure RAEditorGetLineAttr(Sender: TJvEditor; var Line: S' +
            'tring;'
          '      index: Integer; var Attrs: TLineAttrs);'
          
            '    procedure RAEditorPaintGutter(Sender: TJvEditor; Canvas: TCa' +
            'nvas);'
          '    procedure PageControl1Change(Sender: TObject);'
          '    procedure PageControl1Enter(Sender: TObject);'
          '    procedure RAEditorCompletionDrawItem(Control: TWinControl;'
          '      Index: Integer; Rect: TRect; State: TOwnerDrawState);'
          '  private'
          '    Parser : TJvIParser;'
          '  end;'
          ''
          'var'
          '  Editor: TJvEditor ;'
          ''
          'implementation'
          ''
          'uses RAUtilsW;'
          ''
          '{$R *.DFM}'
          ''
          
            'procedure TJvEditor .RAEditorGetLineAttr(Sender: TJvEditor; var ' +
            'Line: String;'
          '  index: Integer; var Attrs: TLineAttrs);'
          ''
          '  procedure SetColor(FC, BC : TColor; Style : TFontStyles);'
          '  var'
          '    i : integer;'
          '  begin'
          '    for i := Parser.PosBeg[0]+1 to Parser.PosEnd[0] do begin'
          '      Attrs[i].FC := FC;'
          '      Attrs[i].BC := BC;'
          '      Attrs[i].Style := Style;'
          '    end;'
          '  end;'
          'var'
          '  Token : string;'
          '  i : integer;'
          'const'
          '  Symbols = ['#39','#39', '#39':'#39', '#39';'#39', '#39'.'#39', '#39'['#39', '#39']'#39', '#39'('#39', '#39')'#39', '#39'='#39', '#39'+'#39','
          '             '#39'-'#39', '#39'/'#39', '#39'<'#39', '#39'>'#39', '#39'%'#39', '#39'*'#39', '#39'~'#39', '#39#39#39#39', '#39'\'#39', '#39'^'#39'];'
          'const'
          '  DelphiKeyWords : string ='
          '    '#39'  constructor  destructor  record  procedure  with  of  '#39'+'
          
            '    '#39'repeat  until  try  finally  except  for  to  downto  case ' +
            ' '#39'+'
          
            '    '#39'type  interface  implementation  initialization  finalizati' +
            'on  '#39'+'
          
            '    '#39'default  private  public  protected  published   automated ' +
            ' property  '#39'+'
          '    '#39'program  read  write  override  object  nil  raise  '#39'+'
          
            '    '#39'on  set  xor  shr  shl  begin  end  args  if  then  else  '#39 +
            '+'
          
            '    '#39'endif  goto  while  do  var  true  false  or  and  not  mod' +
            '  unit  '#39'+'
          '    '#39'function  uses  external  const  class  inherited  '#39
          '  ;'
          ''
          '  BuilderKeyWords : string ='
          
            '    '#39' __asm  _asm  asm  auto  __automated  break  bool  case  ca' +
            'tch  __cdecl  '#39'+'
          
            '    '#39'_cdecl  cdecl  char  class  __classid  __closure  const  co' +
            'nst_cast  '#39'+'
          
            '    '#39'continue  __declspec  default  delete  __dispid  do  double' +
            '  dynamic_cast  '#39'+'
          
            '    '#39'else  enum  __except  explicit  _export  __export  extern  ' +
            'false  __fastcall  '#39'+'
          
            '    '#39'_fastcall  __finally  float  for  friend  goto  if  __impor' +
            't  _import  inline  '#39'+'
          
            '    '#39'int  __int8  __int16  __int32  __int64  long  mutable  name' +
            'space  new  operator  '#39'+'
          
            '    '#39'__pascal  _pascal  pascal  private  protected  __property  ' +
            'public  __published  '#39'+'
          
            '    '#39'register  reinterpret_cast  return  __rtti  short  signed  ' +
            'sizeof  static  static_cast  '#39'+'
          
            '    '#39'__stdcall  _stdcall  struct  switch  template  this  __thre' +
            'ad  throw  true  __try  '#39'+'
          
            '    '#39'try  typedef  typename  typeid  union  using  unsigned  vir' +
            'tual  void  volatile  '#39'+'
          '    '#39'wchar_t  while  '#39
          '  ;'
          ''
          '  SQLKeyWords : string ='
          
            '    '#39'  active  as  add  asc  after  ascending  all  at  alter  a' +
            'uto  '#39'+'
          '    '#39'and  autoddl  any  avg  based  between  basename  blob  '#39'+'
          
            '    '#39'base_name  blobedit  before  buffer  begin  by  cache   com' +
            'piletime  '#39'+'
          
            '    '#39'cast   computed  char   close  character   conditional  cha' +
            'racter_length   connect  '#39'+'
          
            '    '#39'char_length   constraint  check   containing  check_point_l' +
            'en   continue  check_point_length   count  '#39'+'
          
            '    '#39'collate   create  collation   cstring  column   current  co' +
            'mmit   cursor  '#39'+'
          
            '    '#39'committed  database   descending  date   describe  db_key  ' +
            ' descriptor  debug   disconnect  '#39'+'
          
            '    '#39'dec   display  decimal  distinct  declare  do  default   do' +
            'main  '#39'+'
          
            '    '#39'delete   double  desc  drop  echo  exception  edit  execute' +
            '  '#39'+'
          
            '    '#39'else   exists  end   exit  entry_point   extern  escape   e' +
            'xternal  '#39'+'
          
            '    '#39'event   extract  fetch  foreign  file   found  filter   fro' +
            'm  '#39'+'
          
            '    '#39'float   full  for   function  gdscode  grant  generator  gr' +
            'oup  '#39'+'
          
            '    '#39'gen_id  commit_group_wait  global  group_commit_wait_time  ' +
            'goto  '#39'+'
          
            '    '#39'having  help  if   input_type  immediate   insert  in  int ' +
            ' '#39'+'
          
            '    '#39'inactive   integer  index  into  indicator   is  init   iso' +
            'lation  '#39'+'
          '    '#39'inner  isql  input  join  key  '#39'+'
          
            '    '#39'lc_messages   like  lc_type   logfile  left  log_buffer_siz' +
            'e  length  log_buf_size  '#39'+'
          '    '#39'lev   long  level  manual   merge  max   message  '#39'+'
          
            '    '#39'maximum   min  maximum_segment  minimum  max_segment   modu' +
            'le_name  names  not  '#39'+'
          
            '    '#39'national   null  natural   numeric  nchar  num_log_bufs  no' +
            '  num_log_buffers  '#39'+'
          
            '    '#39'noauto  octet_length  or  of   order  on   outer  only  out' +
            'put  '#39'+'
          
            '    '#39'open  output_type  option  overflow  page  post_event  page' +
            'length   precision  '#39'+'
          
            '    '#39'pages   prepare  page_size  procedure  parameter   protecte' +
            'd  password   primary  '#39'+'
          '    '#39'plan   privileges  position   public  quit  '#39'+'
          
            '    '#39'raw_partitions   retain  rdb  db_key   return  read   retur' +
            'ning_values  real   returns  '#39'+'
          
            '    '#39'record_version  revoke  references   right  release   rollb' +
            'ack  reserv  runtime  '#39'+'
          
            '    '#39'reserving  schema   sql  segment   sqlcode  select   sqlerr' +
            'or  set   sqlwarning  '#39'+'
          
            '    '#39'shadow   stability  shared   starting  shell   starts  show' +
            '   statement  '#39'+'
          
            '    '#39'singular   static  size   statistics  smallint   sub_type  ' +
            'snapshot   sum  '#39'+'
          
            '    '#39'some  suspend  sort  table   translate  terminator   transl' +
            'ation  then   trigger  to   trim  '#39'+'
          
            '    '#39'transaction  uncommitted  upper  union   user  unique  usin' +
            'g  update  '#39'+'
          
            '    '#39'value  varying  values  version  varchar  view  variable  '#39 +
            '+'
          '    '#39'wait  while  when  with  whenever  work  where  write  '#39'+'
          ''
          '    '#39'term  new  old '#39
          '   ;'
          ''
          '  function IsDelphiKeyWord(St : string) : boolean;'
          '  begin'
          
            '    Result := Pos('#39' '#39'+ANSILowerCase(St)+'#39' '#39', DelphiKeyWords) <> ' +
            '0;'
          '  end;'
          '  function IsBuilderKeyWord(St : string) : boolean;'
          '  begin'
          '    Result := Pos('#39' '#39'+St+'#39' '#39', BuilderKeyWords) <> 0;'
          '  end;'
          '  function IsSQLKeyWord(St : string) : boolean;'
          '  begin'
          '    Result := Pos('#39' '#39'+ANSILowerCase(St)+'#39' '#39', SQLKeyWords) <> 0;'
          '  end;'
          ''
          '  function IsStringConstant(St : string) : boolean;'
          '  var'
          '    LS : integer;'
          '  begin'
          '    LS := Length(St);'
          
            '    if (LS >= 2) and (((St[1] = '#39#39#39#39') and (St[LS] = '#39#39#39#39')) or ((' +
            'St[1] = '#39'"'#39') and (St[LS] = '#39'"'#39'))) then'
          '      Result := true else'
          '      Result := false'
          '  end;'
          ''
          '  function IsRealConstant(St : string) : boolean;'
          '  var'
          '    i, j : integer;'
          '    Point : boolean;'
          '  begin'
          '    Result := false;'
          '    if (St = '#39'.'#39') or (St = '#39#39') then exit;'
          '    if St[1] = '#39'-'#39' then'
          '      if Length(St) = 1 then exit'
          '      else j := 2'
          '    else j := 1;'
          '    Point := false;'
          '    for i := j to Length(St) do'
          '      if St[i] = '#39'.'#39' then'
          '         if Point then exit else Point := true'
          '      else if (St[i] < '#39'0'#39') or (St[i] > '#39'9'#39') then exit;'
          '    Result := true;'
          '  end;'
          ''
          '  function IsIntConstant(St : string) : boolean;'
          '  var'
          '    i, j : integer;'
          '    Sym : set of char;'
          '  begin'
          '    Result := false;'
          
            '    if (Length(St) = 0) or ((Length(St) = 1) and (St[1]='#39'$'#39')) th' +
            'en exit;'
          '    Sym := StConstSymbols10;'
          '    if (St[1] = '#39'-'#39') or (St[1] = '#39'$'#39') then begin'
          '      if Length(St) = 1 then exit'
          '      else j := 2;'
          '      if St[1] = '#39'$'#39' then Sym := StConstSymbols;'
          '    end else j := 1;'
          '    for i := j to Length(St) do'
          '      if not (St[i] in Sym) then exit;'
          '    Result := true;'
          '  end;'
          ''
          'var'
          '  F : boolean;'
          '  L : integer;'
          '  FC : TColor;'
          '  FS : TFontStyles;'
          'begin'
          '  Parser.pcProgram := PChar(Line);'
          '  Parser.pcPos := Parser.pcProgram;'
          '  L := Sender.Tag;'
          '  if (L = 1) and (Length(Line) > 0) and (Line[1] = '#39'#'#39') then'
          '  begin'
          '    FC := clGreen;'
          '    FS := Sender.Font.Style;'
          '  end else'
          '  begin'
          '    FC := clOlive; {'#228#235#255' '#239#240#232#236#229#247#224#237#232#233'}'
          '    FS := [fsItalic];'
          '  end;'
          '  Attrs[1].FC := FC;'
          '  Attrs[1].Style := FS;'
          '  Attrs[1].BC := Sender.Color;'
          '  for i := 1 to 100 do'
          '     Move(Attrs[1], Attrs[i], sizeof(Attrs[1]));'
          
            '  if (L = 1) and (Length(Line) > 0) and (Line[1] = '#39'#'#39') then exi' +
            't;'
          '  try'
          '    Token := Parser.Token;'
          '    while Token <> '#39#39' do begin'
          '      F := true;'
          '      case L of'
          
            '        0 : if IsDelphiKeyWord(Token) then SetColor(clBlack, Sen' +
            'der.Color, [fsBold]) else F := false;'
          
            '        1 : if IsBuilderKeyWord(Token) then SetColor(clBlack, Se' +
            'nder.Color, [fsBold]) else F := false;'
          
            '        2 : if IsSQLKeyWord(Token) then SetColor(clBlack, Sender' +
            '.Color, [fsBold]) else F := false;'
          '        else F := false;'
          '      end;'
          '      if F then {Ok}'
          '      else if (Length(Token) = 1) and (Token[1] in Symbols) then'
          '        SetColor(clBlue, Sender.Color, [])'
          '      else if IsIntConstant(Token) or IsRealConstant(Token) then'
          '        SetColor(clNavy, Sender.Color, [])'
          '      else if IsStringConstant(Token) then'
          '        SetColor(clPurple, Sender.Color, [])'
          '      else'
          '        SetColor(clBlack, Sender.Color, []);'
          '      Token := Parser.Token;'
          '    end;'
          '  except'
          ''
          '  end;'
          'end;'
          ''
          'procedure TJvEditor .FormCreate(Sender: TObject);'
          'begin'
          '  Parser := TJvIParser.Create;'
          'end;'
          ''
          'procedure TJvEditor .FormDestroy(Sender: TObject);'
          'begin'
          '  Parser.Free;'
          'end;'
          ''
          
            'procedure TJvEditor .RAEditorPaintGutter(Sender: TJvEditor; Canv' +
            'as: TCanvas);'
          '  procedure Draw(Y, ImageIndex : integer);'
          '  var'
          '    Ro : integer;'
          '    R : TRect;'
          '  begin'
          '    if Y <> -1 then'
          '      with Sender do begin'
          '        Ro := Y - TopRow;'
          '        R := CalcCellRect(0, Ro);'
          '        GutterImages.Draw(Canvas,'
          
            '          GutterWidth -GutterRightMargin -GutterImages.Width{R.L' +
            'eft},'
          
            '          R.Top + (CellRect.Height - GutterImages.Height) div 2 ' +
            '+1,'
          '          ImageIndex);'
          '      end;'
          '  end;'
          'var'
          '  i  : integer;'
          'begin'
          '  for i := 0 to 9 do'
          '    if Sender.BookmarksSet[i] then'
          '      Draw(Sender.Bookmarks[i].Y, i);'
          'end;'
          ''
          'procedure TJvEditor .PageControl1Change(Sender: TObject);'
          'begin'
          
            '  Parser.Style := TIParserStyle(not(PageControl1.ActivePage.Page' +
            'Index=0));'
          '  JvEditor.Refresh;'
          '  RAEditor1.Refresh;'
          '  RAEditor2.Refresh;'
          '  RAEditor3.Refresh;'
          'end;'
          ''
          'procedure TJvEditor .PageControl1Enter(Sender: TObject);'
          'begin'
          '  (PageControl1.ActivePage.Controls[0] as TWinControl).SetFocus;'
          'end;'
          ''
          
            'procedure TJvEditor .RAEditorCompletionDrawItem(Control: TWinCon' +
            'trol;'
          '  Index: Integer; Rect: TRect; State: TOwnerDrawState);'
          'var'
          '  Offset, W : Integer;'
          '  S : string;'
          '  ImageIndex : integer;'
          'begin'
          '  Offset := 3;'
          
            '  with Control as TListBox, (Control.Owner as TJvEditor).Complet' +
            'ion do'
          '  begin'
          '    Canvas.FillRect(Rect);'
          '    case Mode of'
          '      cmIdentifers :'
          '        begin'
          
            '          ImageIndex := StrToInt(Trim(SubStr(Items[Index], 2, '#39'^' +
            #39'))) - 1;'
          
            '          ilCompletions.Draw(Canvas, Rect.Left + 2, Rect.Top, Im' +
            'ageIndex);'
          
            '          Canvas.TextOut(Rect.Left + 3*Offset + ilCompletions.Wi' +
            'dth, Rect.Top +2, SubStr(Items[Index], 0, '#39'^'#39'));'
          '          S := Trim(SubStr(Items[Index], 1, '#39'^'#39'));'
          '          W := Canvas.TextWidth(S);'
          
            '          Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2,' +
            ' S);'
          '        end;'
          '      cmTemplates :'
          '        begin'
          
            '          Canvas.TextOut(Rect.Left + Offset, Rect.Top +2, SubStr' +
            '(Items[Index], 1, '#39'^'#39'));'
          '          Canvas.Font.Style := [fsBold];'
          '          S := SubStr(Items[Index], 0, '#39'^'#39');'
          '          W := Canvas.TextWidth(S);'
          
            '          Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2,' +
            ' S);'
          '        end;'
          '    end;'
          '  end;  '
          'end;'
          ''
          'end.')
        GutterWidth = 30
        RightMarginVisible = False
        RightMargin = 0
        RightMarginColor = clSilver
        Completion.Enabled = True
        Completion.ItemHeight = 16
        Completion.Interval = 800
        Completion.ListBoxStyle = lbOwnerDrawFixed
        Completion.Templates.Strings = (
          'arrayd==array declaration (var)==array[0..|] of ;'
          'arrayc==array declaration (const)==array[0..|] of = ();'
          'cases==case statement==case | of/n  : ;/n  : ;/nend;/n'
          
            'casee==case statement (with else)==case | of/n  : ;/n  : ;/nelse' +
            ' ;/nend;/n'
          
            'classf==class declaration (all parts)==T| = class(T)/nprivate/n/' +
            'nprotected/n/npublic/n/npublished/n/nend;/n'
          'classd==class declaration (no parts)==T| = class(T)/n/nend;/n'
          
            'classc==class declaration (with Create/Destroy overrides)==T| = ' +
            'class(T)/nprivate/n/nprotected/n/npublic/n  constructor Create; ' +
            'override;/n  destructor Destroy; override;/npublished/n/nend;/n'
          'fors==for (no begin/end)==for | :=  to  do/n'
          'forb==for statement==for | :=  to  do/nbegin/n/nend;/n')
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '=='
        TabStops = '3 5'
        SmartTab = False
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        OnCompletionDrawItem = RAEditorCompletionDrawItem
        Align = alClient
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
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Builder'
      object RAEditor1: TJvHLEditor
        Tag = 1
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          
            '//--------------------------------------------------------------' +
            '-------------'
          '#include <vcl\vcl.h>'
          '#pragma hdrstop'
          ''
          '#include "eCaseDoc.h"'
          '#include "fDM.h"'
          '#include "Pickdate.h"'
          '#include "vscount.h"'
          '#include "varticle.h"'
          '#include "Russset.h"'
          '#include "vscriptc.h"'
          '#include "Clshow.h"'
          '#include "taskcfg.h"'
          '#include "fdmb.h"'
          '#include "Acond_b.h"'
          '#include "errex.h"'
          
            '//--------------------------------------------------------------' +
            '-------------'
          '#pragma link "Grids"'
          '#pragma resource "*.dfm"'
          'TEditCaseDoc *EditCaseDoc;'
          
            '//--------------------------------------------------------------' +
            '-------------'
          '__fastcall TEditCaseDoc::TEditCaseDoc(TComponent* Owner)'
          '  : TForm(Owner)'
          '{'
          '  NeedFilter = !DM->tSubCount->Filtered;'
          '  if (NeedFilter) {'
          '    DM->tSubCount->Filter="STAT <> '#39'm'#39'";'
          '    DM->tSubCount->Filtered = true;'
          '    DM->tSubCount->Refresh();'
          '  }'
          '  defDate = Date();'
          '}'
          
            '//--------------------------------------------------------------' +
            '-------------'
          '__fastcall TEditCaseDoc::~TEditCaseDoc()'
          '{'
          '  if (NeedFilter) {'
          '    DM->tSubCount->Filter="";'
          '    DM->tSubCount->Filtered = false;'
          '  }'
          '}'
          
            '//--------------------------------------------------------------' +
            '-------------'
          
            'void __fastcall TEditCaseDoc::DBEnumKeyDown(TObject *Sender, WOR' +
            'D &Key,'
          '  TShiftState Shift)'
          '{'
          '  TShiftState Shift1(Shift);'
          '   Shift1.Clear();'
          
            '  if ((Shift == Shift1 || Shift.Contains(ssShift)) && (Key == VK' +
            '_RETURN)){'
          '     Key = 0;'
          '     bool toForward(!Shift.Contains(ssShift));'
          
            '     TWinControl* vv = FindNextControl( ActiveControl, toForward' +
            ', true, false);'
          '     if (vv)'
          '       vv->SetFocus();'
          '   }'
          '  if (Key == VK_SPACE || Key == VK_DOWN || Key == VK_UP) {'
          '    Key = 0;'
          '  }'
          '}'
          
            '//--------------------------------------------------------------' +
            '-------------'
          'void __fastcall TEditCaseDoc::DBEarticleChange(TObject *Sender)'
          '{'
          '  if((DBEarticle->DataSource->DataSet->State == dsEdit ||'
          
            '     DBEarticle->DataSource->DataSet->State == dsInsert) && DBEa' +
            'rticle->Text == "")'
          
            '    DBEarticle->DataSource->DataSet->FieldByName(DBEarticle->Dat' +
            'aField)->AsVariant = Null;'
          '}'
          
            '//--------------------------------------------------------------' +
            '-------------'
          'void __fastcall TEditCaseDoc::SBCal1Click(TObject *Sender)'
          '{'
          '  try{'
          '      BrDateForm->Date = StrToDate(MEdateorder->Text);'
          '  }catch(EConvertError &E){'
          '     BrDateForm->Date = TDateTime(0).CurrentDate();'
          '  }'
          '  if (BrDateForm->ShowModal() == mrOk){'
          '    MEdateorder->Text = DateToStr(BrDateForm->Date);'
          '    ActiveControl = DBEsubcount;'
          '  }'
          '}'
          '')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 0
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 16
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.Templates.Strings = (
          'builder=builder sample=Don'#39't use builder - use DELPHI !')
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlCBuilder
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'SQL'
      object RAEditor2: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          'CONNECT "" USER "" PASSWORD "";'
          ''
          '        /* '#206' '#209' '#205' '#206' '#194' '#205' '#219' '#197'   '#212' '#206' '#205' '#196' '#219' */'
          ''
          'CREATE GENERATOR brt_unicum_gen;'
          'CREATE GENERATOR brtc_unicum_gen;'
          'CREATE GENERATOR bres_unicum_gen;'
          'CREATE GENERATOR brc_unicum_gen;'
          'CREATE GENERATOR bro_unicum_gen;'
          ''
          
            '/*=========== B R _ T Y P E ====================================' +
            '==*/'
          '/* '#210#224#225#235#232#246#224' '#242#232#239#251' '#238#241#237#238#226#237#251#245' '#244#238#237#228#238#226' */'
          '/* Table: br_type  brt */'
          'CREATE TABLE br_type ('
          '        unicum          dIntNN,'
          '        name            dChar250,   /* '#238#239#232#241#224#237#232#229' '#234#240#224#242#234#238#229' */'
          '        PRIMARY KEY (unicum)'
          ');'
          'CREATE INDEX brt_name ON br_type(name);'
          '/*====================================================*/'
          '/* '#210#224#225#235#232#246#224' "'#210#232#239#251' '#240#224#241#247#229#242#238#226'" */'
          '/* Table: br_typeCalc brtc */'
          'CREATE TABLE br_typeCalc ('
          '        unicum          dIntNN,'
          '        name            dChar50, /* '#228#235#255' '#239#240#238#241#236#238#242#240#224' */'
          '        script          dChar50, /* '#228#235#255' '#234#224#240#242#238#247#234#232'  */'
          '        debet           dSc,'
          '        credit          dSc,'
          '        formula         dChar250,'
          '        PRIMARY KEY     (unicum)'
          ');'
          'CREATE INDEX brtc_name ON br_typeCalc(name);'
          'COMMIT WORK;'
          '/*=========== B R E S =====================================*/'
          '/* '#210#224#225#235#232#246#224' "'#206#241#237#238#226#237#251#229' '#244#238#237#228#251'" */'
          '/* Table: bresours bres */'
          'CREATE TABLE bresours ('
          '        unicum          dIntNN,'
          
            '        isActive        dDate,     /* NULL - '#224#234#242#232#226#237#251#233'  DATA - '#241#237 +
            #255#242#232#229' '#241' '#243#247#229#242#224' */'
          
            '        stat            dInt,      /* 0 - '#234#224#240#242#238#247#234#224' 1- '#227#240#243#239#239#238#226#224#255' ' +
            '2- '#247#235#229#237' '#227#240#243#239#239#251'*/'
          
            '        uSelf           dInt,      /* '#241#241#251#235#234#224' '#237#224' unicum '#227#240#243#239#239#251' '#228#235 +
            #255' '#247#235#229#237#238#226' '#227#240#243#239#239#251' */'
          '        type            dIntNN,    /* '#242#232#239' */'
          
            '        card            dChar15,   /* '#237#238#236#229#240' '#232#237#226#229#237#242#224#240#237#238#233' '#234#224#240#242#238#247#234#232 +
            ' */'
          '        inv             dChar15,   /* '#232#237#226#229#237#242#224#240#237#251#233' '#237#238#236#229#240' */'
          '        name            dChar250,  /* '#237#224#232#236#229#237#238#226#224#237#232#229' */'
          '        place           dChar250,  /* '#236#229#241#242#238#237#224#245#238#230#228#229#237#232#229' */'
          
            '        dateBorn        dDate,     /* '#228#224#242#224' '#226#251#239#243#241#234#224' ('#226#226#238#228#224' '#226' '#253#234#241#239 +
            #235#243#224#242#224#246#232#254') */'
          '        dateReg         dDate,     /* '#228#224#242#224' '#239#238#241#242#224#237#238#226#234#232' '#237#224' '#243#247#229#242' */'
          
            '        summaReg        dMoneyS,   /* '#239#229#240#226#238#237#224#247#224#235#252#237#224#255' '#241#242#238#232#236#238#241#242#252' *' +
            '/'
          '        league          dInt,      /* '#243#247#229#242#237#224#255' '#227#240#243#239#239#224' */'
          '        subcount        dSc,       /* '#225#224#235#224#237#241#238#226#251#233' '#241#247#229#242' */'
          
            '        fond            dSc,       /* '#241#247#229#242' '#244#238#237#228#224' '#226' '#238#241#237#238#226#237#251#245' '#241#240#229#228 +
            #241#242#226#224#245' NEW */'
          '        amort           dSc,       /* '#241#247#229#242' '#224#236#238#240#242#232#231#224#246#232#232' NEW */'
          '        PRIMARY KEY (unicum),'
          '        FOREIGN KEY (subcount) REFERENCES subcount(num),'
          '        FOREIGN KEY (fond)     REFERENCES subcount(num),'
          '        FOREIGN KEY (amort)    REFERENCES subcount(num),'
          '        FOREIGN KEY (league)   REFERENCES league(unicum),'
          '        FOREIGN KEY (uself)    REFERENCES bresours(unicum),'
          '        FOREIGN KEY (type)     REFERENCES br_type(unicum)'
          ');'
          'CREATE INDEX bres_name ON bresours(name);'
          'CREATE INDEX bres_inv  ON bresours(inv);'
          'CREATE INDEX bres_card ON bresours(card);'
          'CREATE INDEX bres_sc   ON bresours(subcount);'
          '/* CREATE INDEX bres_sc   ON bresours(subcount,name); */'
          'COMMIT WORK;'
          '/*====================================================*/'
          '/* '#210#224#225#235#232#246#224' "'#196#238#239'. '#228#224#237#237#251#229'" */'
          '/* Table: br_changed  brc */'
          'CREATE TABLE br_changed ('
          '        ubres           dIntNN,    /* '#241#241#251#235#234#224' '#237#224' unicum        */'
          '        dateCh          dDateNN,   /* '#228#224#242#224' '#232#231#236#229#237#229#237#232#255'          */'
          '        dateOut         dDate,     /* '#241#240#238#234' '#241#239#232#241#224#237#232#255'           */'
          '        fst_summa       dMoneyS,   /* '#243#247#229#242#237#224#255' '#241#242#238#232#236#238#241#242#252'       */'
          '        am_summa        dMoneyS,   /* '#241#243#236#236#224' '#232#231#237#238#241#224'            */'
          '        uclient         dInt,      /* '#238#242#226#229#242#241#242#226#229#237#237#251#233'           */'
          '        typeAm          dInt,      /* '#242#232#239' '#240#224#241#247#229#242#224' '#224#236#238#240#242#232#231#224#246#232#232' */'
          '        typeRa          dInt,      /* '#242#232#239' '#240#224#241#247#229#242#224' '#239#229#240#229#238#246#229#237#234#232'  */'
          '        param1          dChar15,   /* '#239#224#240#224#236#229#242#240#251'               */'
          '        param2          dChar15,'
          '        param3          dChar15,'
          '        param4          dChar15,'
          '        param5          dChar15,   /*..................*/'
          '        ubk             dInt,'
          '        PRIMARY KEY (ubres,dateCh),'
          '        FOREIGN KEY (ubres)    REFERENCES bresours(unicum),'
          '        FOREIGN KEY (typeAm)   REFERENCES br_typeCalc(unicum),'
          '        FOREIGN KEY (typeRa)   REFERENCES br_typeCalc(unicum),'
          '        FOREIGN KEY (uclient)  REFERENCES client (unicum)'
          ');'
          'COMMIT WORK;'
          '/*====================================================*/'
          '/* '#210#224#225#235#232#246#224' "'#206#239#229#240#224#246#232#232'" */'
          '/* Table: br_oper  bro */'
          'CREATE TABLE br_oper ('
          '        unicum          dIntNN,'
          '        dateOper        dDate,     /* '#228#224#242#224' '#238#239#229#240#224#246#232#232'           */'
          '        ubres           dIntNN,    /* unicum '#238#241#237#238#226#237#238#227#238' '#244#238#237#228#224'  */'
          '        type            dInt,      /* '#242#232#239' '#240#224#241#247#229#242#224'             */'
          '        debet           dScNN,'
          '        credit          dScNN,'
          '        summa           dMoneyS,   /* '#241#243#236#236#224'                   */'
          '        ubk             dInt,'
          '        PRIMARY KEY     (unicum),'
          '        FOREIGN KEY (ubres)        REFERENCES bresours(unicum),'
          
            '        FOREIGN KEY (type)         REFERENCES br_typeCalc(unicum' +
            ')'
          ');'
          'CREATE INDEX brc_oper ON br_oper(ubres,dateOper);'
          'COMMIT WORK;'
          
            '/*======================= STORE PROCEDUREs =====================' +
            '======*/'
          'SET TERM  ^ ;'
          
            '/*--------------------------------------------------------------' +
            '------*/'
          'CREATE ALTER PROCEDURE br_view('
          '       act       INTEGER,'
          '       gr        INTEGER,'
          '       sc        CHAR(6),'
          '       lg        INTEGER)'
          'RETURNS('
          '        /* from BRES */'
          '        f_unicum          INTEGER,'
          
            '        f_isActive        DATE,          /* NULL - '#224#234#242#232#226#237#251#233'  DAT' +
            'A - '#241#237#255#242#232#229' '#241' '#243#247#229#242#224' */'
          
            '        f_stat            INTEGER,       /* 0 - '#234#224#240#242#238#247#234#224' 1- '#227#240#243#239 +
            #239#238#226#224#255' 2- '#247#235#229#237' '#227#240#243#239#239#251'*/'
          
            '        f_uSelf           INTEGER,       /* '#241#241#251#235#234#224' '#237#224' unicum '#227#240#243 +
            #239#239#251' '#228#235#255' '#247#235#229#237#238#226' '#227#240#243#239#239#251' */'
          '        f_type            INTEGER,       /* '#242#232#239' */'
          
            '        f_card            CHAR(15),      /* '#237#238#236#229#240' '#232#237#226#229#237#242#224#240#237#238#233' '#234#224 +
            #240#242#238#247#234#232' */'
          '        f_inv             CHAR(15),      /* '#232#237#226#229#237#242#224#240#237#251#233' '#237#238#236#229#240' */'
          '        f_name            CHAR(250),     /* '#237#224#232#236#229#237#238#226#224#237#232#229' */'
          '        f_place           CHAR(250),     /* '#236#229#241#242#238#237#224#245#238#230#228#229#237#232#229' */'
          
            '        f_dateBorn        DATE,          /* '#228#224#242#224' '#226#251#239#243#241#234#224' ('#226#226#238#228#224' ' +
            #226' '#253#234#241#239#235#243#224#242#224#246#232#254') */'
          
            '        f_dateReg         DATE,          /* '#228#224#242#224' '#239#238#241#242#224#237#238#226#234#232' '#237#224' '#243 +
            #247#229#242' */'
          
            '        f_summaReg        NUMERIC(15,2), /* '#239#229#240#226#238#237#224#247#224#235#252#237#224#255' '#241#242#238#232#236 +
            #238#241#242#252' */'
          '        f_league          INTEGER,       /* '#243#247#229#242#237#224#255' '#227#240#243#239#239#224' */'
          '        f_subcount        CHAR(6),       /* '#225#224#235#224#237#241#238#226#251#233' '#241#247#229#242' */'
          
            '        f_fond            CHAR(6),       /* '#241#247#229#242' '#226' '#238#241#237#238#226#237#251#245' '#241#240#229#228 +
            #241#242#226#224#245' */'
          '        f_amort           CHAR(6),       /* '#241#247#229#242' '#224#236#238#240#242#232#231#224#246#232#232' */'
          '        /* from BR_CHANGED */'
          
            '        s_ubres           INTEGER,       /* '#241#241#251#235#234#224' '#237#224' unicum    ' +
            '     */'
          
            '        s_dateCh          DATE,          /* '#228#224#242#224' '#232#231#236#229#237#229#237#232#255'      ' +
            '     */'
          
            '        s_dateOut         DATE,          /* '#228#224#242#224' '#241#239#232#241#224#237#232#255'       ' +
            '     */'
          
            '        s_fst_summa       NUMERIC(15,2), /* '#239#229#240#226#238#237#224#247#224#235#252#237#224#255' '#241#242#238#232#236 +
            #238#241#242#252' */'
          
            '        s_am_summa        NUMERIC(15,2), /* '#239#229#240#226#238#237#224#247#224#235#252#237#224#255' '#241#242#238#232#236 +
            #238#241#242#252' */'
          
            '        s_uclient         INTEGER,       /* '#238#242#226#229#242#241#242#226#229#237#237#251#233'       ' +
            '     */'
          
            '        s_typeAm          INTEGER,       /* '#242#232#239' '#240#224#241#247#229#242#224' '#224#236#238#240#242#232#231#224 +
            #246#232#232'  */'
          
            '        s_typeRa          INTEGER,       /* '#242#232#239' '#240#224#241#247#229#242#224' '#239#229#240#229#238#246#229#237 +
            #234#232'   */'
          
            '        s_param1          CHAR(15),      /* '#239#224#240#224#236#229#242#240#251'           ' +
            '     */'
          '        s_param2          CHAR(15),'
          '        s_param3          CHAR(15),'
          '        s_param4          CHAR(15),'
          
            '        s_param5          CHAR(15)       /*.....................' +
            '....*/'
          '       )'
          'AS'
          'BEGIN'
          '     FOR SELECT unicum,   isActive, stat,     uSelf,'
          '                type,     card,     inv,      name,   place,'
          '                dateBorn, dateReg,  summaReg,'
          '                league, subcount, fond, amort'
          '         FROM   bresours'
          '         WHERE  ( ( :act = 0  OR'
          '                    (:act = 1 AND isActive IS NULL) OR'
          '                    (:act = 2 AND isActive IS NOT NULL) ) AND'
          '                  ( :gr = -1  OR type     = :gr) AND'
          '                  ( :sc = "*" OR subcount = :sc) AND'
          '                  ( :lg = -1  OR league   = :lg)'
          '                )'
          '         INTO   :f_unicum,   :f_isActive, :f_stat,     :f_uSelf,'
          
            '                :f_type,     :f_card,     :f_inv,      :f_name, ' +
            '  :f_place,'
          '                :f_dateBorn, :f_dateReg,  :f_summaReg,'
          '                :f_league,   :f_subcount, :f_fond,     :f_amort'
          '     DO BEGIN'
          
            '        SELECT  ubres,  dateCh, dateOut, fst_summa, am_summa, uc' +
            'lient,'
          
            '                typeAm, typeRa, param1, param2, param3, param4, ' +
            'param5'
          '        FROM    br_changed'
          '        WHERE   (ubres = :f_unicum AND '
          
            '                 dateCh = (SELECT MAX(dateCh) FROM br_changed WH' +
            'ERE ubres = :f_unicum))'
          '        ORDER BY  dateCh DESC'
          
            '        INTO :s_ubres,  :s_dateCh, :s_dateOut, :s_fst_summa, :s_' +
            'am_summa, :s_uclient,'
          
            '             :s_typeAm, :s_typeRa, :s_param1, :s_param2, :s_para' +
            'm3, :s_param4, :s_param5;'
          '        SUSPEND;'
          '     END'
          ' EXIT;'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'CREATE PROCEDURE bres_insert             /* 001 */'
          'RETURNS(f_unicum          INTEGER)'
          'AS'
          'DECLARE VARIABLE t_uType INTEGER;'
          'BEGIN'
          ' SELECT MAX(unicum) FROM br_type INTO :t_uType;'
          ' IF (:t_uType IS NULL) THEN'
          ' BEGIN'
          
            '    INSERT INTO br_type VALUES (0,"'#205#229#238#239#240#229#228#229#235#229#237#237#224#255' '#227#240#243#239#239#224' ('#236'. '#243#228#224 +
            #235#232#242#252')");'
          '    t_uType = 0;'
          ' END'
          ' f_unicum = GEN_ID(bres_unicum_gen, 1);'
          ' INSERT INTO bresours(unicum,type) VALUES (:f_unicum,:t_uType);'
          ' EXIT;'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'COMMIT WORK ^'
          'SET TERM ; ^'
          
            '/*==============================================================' +
            '====*/'
          
            '/*============= T R I G G E R S ================================' +
            '====*/'
          
            '/*==============================================================' +
            '====*/'
          ''
          
            '/*======= B R _ T Y P E  =======================================' +
            '=====*/'
          'SET TERM ^ ;'
          ''
          'CREATE TRIGGER brt_insert_bf FOR br_type'
          'ACTIVE BEFORE INSERT POSITION 0'
          'AS'
          'BEGIN'
          '    IF (NEW.unicum IS NULL) THEN'
          '       NEW.unicum = GEN_ID(brt_unicum_gen, 1);'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'CREATE TRIGGER brt_insert_af FOR br_type'
          'ACTIVE AFTER INSERT POSITION 0'
          'AS'
          'BEGIN'
          '    POST_EVENT "BRT_CHANGED";'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'CREATE TRIGGER brt_update_af FOR br_type'
          'ACTIVE AFTER UPDATE POSITION 0'
          'AS'
          'BEGIN'
          '    POST_EVENT "BRT_CHANGED";'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'CREATE TRIGGER brt_delete_af FOR br_type'
          'ACTIVE AFTER DELETE POSITION 0'
          'AS'
          'BEGIN'
          '    POST_EVENT "BRT_CHANGED";'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'COMMIT WORK ^'
          'SET TERM ; ^'
          ''
          
            '/*======= B R _ T Y P E C A L C ================================' +
            '==*/'
          'SET TERM ^ ;'
          ''
          'CREATE TRIGGER brtc_insert_bf FOR br_typeCalc'
          'ACTIVE BEFORE INSERT POSITION 0'
          'AS'
          'BEGIN'
          '    IF (NEW.unicum IS NULL) THEN'
          '       NEW.unicum = GEN_ID(brtc_unicum_gen, 1);'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'CREATE TRIGGER brtc_insert_af FOR br_typeCalc'
          'ACTIVE AFTER INSERT POSITION 0'
          'AS'
          'BEGIN'
          '    POST_EVENT "BRTC_CHANGED";'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'CREATE TRIGGER brtc_update_af FOR br_typeCalc'
          'ACTIVE AFTER UPDATE POSITION 0'
          'AS'
          'BEGIN'
          '    POST_EVENT "BRTC_CHANGED";'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'CREATE TRIGGER brtc_delete_af FOR br_typeCalc'
          'ACTIVE AFTER DELETE POSITION 0'
          'AS'
          'BEGIN'
          '    POST_EVENT "BRTC_CHANGED";'
          'END'
          ' ^'
          
            '/*--------------------------------------------------------------' +
            '--*/'
          'COMMIT WORK ^'
          'SET TERM ; ^'
          '')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 0
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.Templates.Strings = (
          'crp=create procedure=create procedure | as/nbegin/nend')
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlSql
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Python'
      object RAHLEditor1: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          '# Dialog.py -- Tkinter interface to the tk_dialog script.'
          ''
          'from Tkinter import *'
          'from Tkinter import _cnfmerge'
          ''
          'if TkVersion <= 3.6:'
          '  DIALOG_ICON = '#39'warning'#39
          'else:'
          '  DIALOG_ICON = '#39'questhead'#39
          ''
          ''
          'class Dialog(Widget):'
          '  def __init__(self, master=None, cnf={}, **kw):'
          '    cnf = _cnfmerge((cnf, kw))'
          '    self.widgetName = '#39'__dialog__'#39
          '    Widget._setup(self, master, cnf)'
          '    self.num = self.tk.getint('
          '      apply(self.tk.call,'
          '            ('#39'tk_dialog'#39', self._w,'
          '             cnf['#39'title'#39'], cnf['#39'text'#39'], '
          '             cnf['#39'bitmap'#39'], cnf['#39'default'#39'])'
          '            + cnf['#39'strings'#39']))'
          '    try: Widget.destroy(self)'
          '    except TclError: pass'
          '  def destroy(self): pass'
          ''
          'def _test():'
          '  d = Dialog(None, {'#39'title'#39': '#39'File Modified'#39','
          '        '#39'text'#39':'
          '        '#39'File "Python.h" has been modified'#39
          '        '#39' since the last time it was saved.'#39
          '        '#39' Do you want to save it before'#39
          '        '#39' exiting the application.'#39','
          '        '#39'bitmap'#39': DIALOG_ICON,'
          '        '#39'default'#39': 0,'
          '        '#39'strings'#39': ('#39'Save File'#39', '
          '              '#39'Discard Changes'#39', '
          '              '#39'Return to Editor'#39')})'
          '  print d.num'
          ''
          ''
          'if __name__ == '#39'__main__'#39':'
          '  t = Button(None, {'#39'text'#39': '#39'Test'#39','
          '        '#39'command'#39': _test,'
          '        Pack: {}})'
          '  q = Button(None, {'#39'text'#39': '#39'Quit'#39','
          '        '#39'command'#39': t.quit,'
          '        Pack: {}})'
          '  t.mainloop()'
          '')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 40
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlPython
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Java'
      object RAHLEditor2: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          '/***********************************************************'
          '*'
          '*                RAJ-Library'
          '*                   RAJI'
          '*       Copyright (C) 2000 JVCL'
          '*'
          '*       description : expression evaluator'
          '*'
          '*       programmer  : Andrey Prigunkov'
          '*       e-mail      : black@infa.ru'
          '*                     ncuxxu@pisem.net'
          '*       www         : http://www.infa.ru/black'
          '************************************************************/'
          ''
          'package ra.ji;'
          ''
          'import java.io.*;'
          ''
          'public class Expression { '
          '  '
          '  private String source;'
          '  '
          '  private final static int MAX_EXP_STACK = 100;'
          '  private int ExpStackPtr;'
          '  private Variant[] ExpStack = new Variant[MAX_EXP_STACK];'
          '  '
          '  protected Variant vResult;'
          '  protected Parser parser;'
          '  protected transient Events events;'
          '  protected Reflector reflector = new Reflector();'
          '  protected Args args = new Args();'
          '  '
          '  public Expression() {'
          '    parser = new Parser(null);'
          '  }'
          ''
          '  public void setSource(String source) {'
          '    this.source = source;'
          '    sourceChanged();'
          '  }'
          ''
          '  /** return result as variant'
          '   */'
          '  public Variant getResult() {'
          '    if (vResult == null) run();'
          '    return vResult;'
          '  }'
          ''
          '  /** trigger, called then source is changed'
          '   */'
          '  public void sourceChanged() {'
          '    vResult = null;'
          '  }'
          '  '
          '  /** push value to stack'
          '   */'
          '  private void pushExp(Variant Value) {'
          '    ExpStackPtr ++;'
          '    if (ExpStackPtr > MAX_EXP_STACK)'
          '      throw new EjiError(EjiError.INTERNAL, -1);'
          '    ExpStack[ExpStackPtr] = new Variant(Value);'
          '  }'
          ''
          '  /** pop value from stack'
          '   */'
          '  private Variant popExp() {'
          '    if (ExpStackPtr < 0)'
          '      throw new EjiError(EjiError.INTERNAL, -1);'
          '    return ExpStack[ExpStackPtr --];'
          '  }'
          ''
          '  /** evaluate subexpression'
          '   */'
          '  private Variant exp(int OpTyp) {'
          '    if (OpTyp != Parser.TT_UNKNOWN)'
          '      parser.nextToken();'
          '    Variant result = new Variant();'
          '    while (true) {'
          '      switch (parser.type) {'
          '        case Parser.TT_POINT:'
          '          parser.nextToken();'
          '          if (parser.type != Parser.TT_IDENTIFER)'
          
            '            throw new EjiError(EjiError.EXPECTED, parser.getPos(' +
            '), EjiError.IR_IDENTIFER);'
          '          if (result.vType != Variant.varObject &&'
          '              result.vType != Variant.varClass)'
          
            '            throw new EjiError(EjiError.OBJECT_REQUIRED, parser.' +
            'getPosBeg());'
          '        case Parser.TT_INTEGER: '
          '        case Parser.TT_DOUBLE:'
          '        case Parser.TT_STRING:'
          '        case Parser.TT_FALSE:'
          '        case Parser.TT_TRUE:              '
          '        case Parser.TT_NULL:'
          '        case Parser.TT_IDENTIFER:'
          '          if (parser.type == Parser.TT_IDENTIFER) {'
          '            args.clear();'
          '            args.identifer = parser.token;'
          '            parser.nextToken();'
          '            if (parser.type == Parser.TT_ASSIGN) {'
          '              iSetValue(result);'
          '            } else {'
          '              iGetValue(result);'
          '            }'
          '          } else {'
          '            result.setVariant(parser.vToken);'
          '            parser.nextToken();'
          '          }'
          '          switch (parser.type) {'
          
            '            case Parser.TT_INTEGER: case Parser.TT_DOUBLE: case ' +
            'Parser.TT_STRING: '
          
            '            case Parser.TT_FALSE: case Parser.TT_TRUE: case Pars' +
            'er.TT_IDENTIFER:'
          
            '              throw new EjiError(EjiError.MISSING_OPERATOR, pars' +
            'er.getPrevPos());'
          '          }'
          '          if (parser.prior(parser.type) < parser.prior(OpTyp)) '
          '            return result;'
          '          break;'
          '        case Parser.TT_NEW:'
          '          result = new1();'
          '          // need check to point !;'
          '          break;'
          '        case Parser.TT_LB:'
          '          result = exp(parser.type);'
          '          if (parser.type != Parser.TT_RB)'
          
            '            throw new EjiError(EjiError.EXPECTED, parser.getPos(' +
            '), "'#39')'#39'");'
          '          parser.nextToken();'
          '          break;'
          '        case Parser.TT_LS:'
          '          result = exp(parser.type);'
          '          if (parser.type != Parser.TT_RS)'
          
            '            throw new EjiError(EjiError.EXPECTED, parser.getPos(' +
            '), "'#39']'#39'");'
          '          parser.nextToken();'
          '          break;'
          '        case Parser.TT_RB:'
          '        case Parser.TT_RS:'
          '          if (result.vType == Variant.varEmpty)'
          
            '            throw new EjiError(EjiError.EXPECTED, parser.getPos(' +
            '), EjiError.IR_EXPRESSION);'
          '          else '
          '            return result;'
          '        case Parser.TT_PLUS:'
          '          switch (parser.prevtype) {'
          
            '            case Parser.TT_INTEGER: case Parser.TT_DOUBLE: case ' +
            'Parser.TT_STRING: '
          
            '            case Parser.TT_FALSE: case Parser.TT_TRUE: case Pars' +
            'er.TT_IDENTIFER:'
          '            case Parser.TT_RB: case Parser.TT_RS: '
          '              if (parser.PRIOR_PLUS > parser.prior(OpTyp))'
          
            '                result = Variant.plus(popExp(), exp(parser.type)' +
            ');'
          '              else '
          '                return result;'
          '              break;'
          '            default:'
          
            '              result = Variant.unarplus(exp(Parser.TT_NOT /*high' +
            'est priority*/));'
          '          }'
          '          break;'
          '        case Parser.TT_MINUS:'
          '          switch (parser.prevtype) {'
          
            '            case Parser.TT_INTEGER: case Parser.TT_DOUBLE: case ' +
            'Parser.TT_STRING: '
          
            '            case Parser.TT_FALSE: case Parser.TT_TRUE: case Pars' +
            'er.TT_IDENTIFER:'
          '            case Parser.TT_RB: case Parser.TT_RS: '
          '              if (parser.PRIOR_MINUS > parser.prior(OpTyp))'
          
            '                result = Variant.minus(popExp(), exp(parser.type' +
            '));'
          '              else '
          '                return result;'
          '              break;'
          '            default:'
          
            '              result = Variant.unarminus(exp(Parser.TT_NOT /*hig' +
            'hest priority*/));'
          '            }'
          '          break;'
          '        case Parser.TT_MUL:'
          '          if (parser.PRIOR_MUL > parser.prior(OpTyp))'
          '            result = Variant.mul(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_DIV:'
          '          if (parser.PRIOR_DIV > parser.prior(OpTyp))'
          '            result = Variant.div(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_MOD:'
          '          if (parser.PRIOR_MOD > parser.prior(OpTyp))'
          '            result = Variant.mod(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_AND:'
          '          if (parser.PRIOR_AND > parser.prior(OpTyp))'
          '            result = Variant.and(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_OR:'
          '          if (parser.PRIOR_OR > parser.prior(OpTyp))'
          '            result = Variant.or(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_NOT:'
          '          if (parser.PRIOR_NOT > parser.prior(OpTyp))'
          '            result = Variant.not(exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_BITAND:'
          '          if (parser.PRIOR_BITAND > parser.prior(OpTyp))'
          '            result = Variant.bitand(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_BITOR:'
          '          if (parser.PRIOR_BITOR > parser.prior(OpTyp))'
          '            result = Variant.bitor(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_BITNOT:'
          '          if (parser.PRIOR_NOT > parser.prior(OpTyp))'
          '            result = Variant.bitnot(exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_EQU:'
          '          if (parser.PRIOR_EQU > parser.prior(OpTyp))'
          '            result = Variant.equ(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_LESS:'
          '          if (parser.PRIOR_LESS > parser.prior(OpTyp))'
          '            result = Variant.less(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_GREATER:'
          '          if (parser.PRIOR_GREATER > parser.prior(OpTyp))'
          
            '            result = Variant.greater(popExp(), exp(parser.type))' +
            ';'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_NOTEQU:'
          '          if (parser.PRIOR_NOTEQU > parser.prior(OpTyp))'
          '            result = Variant.notequ(popExp(), exp(parser.type));'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_EQULESS:'
          '          if (parser.PRIOR_EQULESS > parser.prior(OpTyp))'
          
            '            result = Variant.equless(popExp(), exp(parser.type))' +
            ';'
          '          else '
          '            return result;'
          '          break;'
          '        case Parser.TT_EQUGREATER:'
          '          if (parser.PRIOR_EQUGREATER > parser.prior(OpTyp))'
          
            '            result = Variant.equgreater(popExp(), exp(parser.typ' +
            'e));'
          '          else '
          '            return result;'
          '          break;'
          '        '
          '        default:'
          '          if (result.vType == Variant.varEmpty)'
          
            '            throw new EjiError(EjiError.EXPECTED, parser.getPos(' +
            '), EjiError.IR_EXPRESSION);'
          '          else '
          '            return result;'
          '      }    '
          '      pushExp(result);'
          '    }'
          '  }'
          '  '
          '  /** evaluate expression up to a first unrecognized token'
          '   */'
          '  public Variant expression1() {'
          '    int OldExpStackPtr;'
          '    try {'
          '      OldExpStackPtr = ExpStackPtr;'
          '      try {'
          '        exp(Parser.TT_UNKNOWN);'
          '        return popExp();'
          '      } finally {'
          '        ExpStackPtr = OldExpStackPtr;'
          '      }'
          '    } catch (EjiError e) {'
          '      if (e.errPos == -1)'
          '        e.errPos = parser.getPosBeg();'
          '      if (e.errLine == -1)'
          '        e.errLine = parser.getLineBeg();'
          '      throw e;'
          '    }'
          '  }'
          ''
          '  /** evaluate expression, '
          '   * check type of result with given expTyp'
          '   */'
          '  public Variant expression2(int expTyp) {'
          '    Variant result = expression1();'
          '    if (result.vType != expTyp)'
          '      switch (expTyp) {'
          '        case Variant.varInteger:'
          
            '          throw new EjiError(EjiError.INTEGER_REQUIRED, parser.g' +
            'etPos());'
          '        case Variant.varBoolean:'
          
            '          throw new EjiError(EjiError.BOOLEAN_REQUIRED, parser.g' +
            'etPos());'
          '        default:'
          
            '          throw new EjiError(EjiError.INTERNAL, parser.getPos())' +
            ';'
          '      }'
          '    return result;'
          '  }'
          ''
          '  public void setEvents(Events events) {'
          '    this.events = events;'
          '  }'
          ''
          '  /** read arguments for function call'
          '   */'
          '  private void readArgs() {'
          '    Args localArgs = args;'
          '    args = new Args();'
          '    try {'
          '      int SK;'
          '      if (parser.type == Parser.TT_LB) '
          '        SK = Parser.TT_RB;'
          '      else // (parser.type == Parser.TT_LS) '
          '        SK = Parser.TT_RS;'
          '      parser.nextToken();'
          '      args.clear();'
          ''
          '      int i = 0;'
          ''
          '      if (parser.type != Parser.TT_RB &&'
          '          parser.type != Parser.TT_RS) {'
          '        localArgs.values[i ++].setVariant(expression1());'
          '        while (parser.type == Parser.TT_COL) {'
          '          parser.nextToken();'
          '          args.clear();'
          '          localArgs.values[i ++].setVariant(expression1());'
          '        }'
          '      }'
          '      '
          '      if (parser.type != SK) '
          '        if (SK == Parser.TT_RB)'
          
            '          throw new EjiError(EjiError.EXPECTED, parser.getPosBeg' +
            '(), "'#39')'#39'");'
          '        else'
          
            '          throw new EjiError(EjiError.EXPECTED, parser.getPosBeg' +
            '(), "'#39']'#39'");'
          '      parser.nextToken();'
          '      localArgs.count = i;'
          '    } finally { '
          '      args = localArgs;'
          '    }'
          '  }'
          '  '
          '  /** find value for a specified identifer'
          '   */'
          '  private void iGetValue(Variant value) {'
          '    if (parser.type == Parser.TT_LB ||'
          '        parser.type == Parser.TT_LS)'
          '      readArgs();'
          '    else'
          '      args.count = -1;'
          '    args.pointer = new Variant(value);'
          '    /*if (!getValue(value)) '
          
            '      throw new EjiError(EjiError.UNKNOWN_IDENTIFER, parser.getP' +
            'revPos(), args.identifer);*/'
          '    if (!getValue(value)) {'
          '      int errPos = parser.getPrevPos();'
          '      String errIdentifer = new String(args.identifer);'
          '      if (args.count == -1 && parser.type == Parser.TT_POINT) {'
          '        // trying to find name of package'
          '        boolean found = false;'
          '        String identifer = null;'
          '        while (parser.type == Parser.TT_POINT ||'
          '               parser.type == Parser.TT_IDENTIFER) {'
          '          args.identifer += parser.token;'
          '          if (parser.type == Parser.TT_IDENTIFER &&'
          '              reflector.getClass(args, value)) {'
          '            found = true;'
          '            break;'
          '          }'
          '          parser.nextToken();'
          '        }'
          '        if (!found)'
          
            '          throw new EjiError(EjiError.UNKNOWN_IDENTIFER, errPos,' +
            ' errIdentifer);'
          '        parser.nextToken();'
          '      } else'
          
            '        throw new EjiError(EjiError.UNKNOWN_IDENTIFER, errPos, e' +
            'rrIdentifer);'
          '    }'
          '    args.pointer = null;'
          '    args.count = 0;'
          '  }'
          ''
          '  /** set value for a specified identifer'
          '   */'
          '  private void iSetValue(Variant value) {'
          '    args.pointer = new Variant(value);'
          '    parser.nextToken();'
          '    Args oldArgs = args;'
          '    try {'
          '      args = new Args();'
          '      value.setVariant(expression1());'
          '    } finally {'
          '      args = oldArgs;'
          '    }'
          '    if (!setValue(value))'
          
            '      throw new EjiError(EjiError.UNKNOWN_IDENTIFER, parser.getP' +
            'revPos(), args.identifer);'
          '  }'
          '  '
          '  protected boolean getValue(Variant result) {'
          '    return ((events != null && events.getValue(args, result)) ||'
          '            reflector.getValue(args, result));'
          '  }'
          ''
          '  protected boolean setValue(Variant value) {'
          '    return ((events != null && events.setValue(args, value)) ||'
          '            reflector.setValue(args, value));'
          '  }'
          '  '
          '  /** create instance of object'
          '   */'
          '  private Variant new1() {'
          '    Variant result = new Variant();'
          '    parser.nextToken();'
          '    if (parser.type != Parser.TT_IDENTIFER)'
          
            '      throw new EjiError(EjiError.EXPECTED, parser.getPos(), Eji' +
            'Error.IR_IDENTIFER);'
          '    args.clear();'
          '    args.identifer = parser.token;'
          '    parser.nextToken();'
          '    while (parser.type == Parser.TT_POINT ||'
          '           parser.type == Parser.TT_IDENTIFER) {'
          '      args.identifer += parser.token;'
          '      parser.nextToken();'
          '    }'
          '    if (parser.type != Parser.TT_LB)'
          
            '      throw new EjiError(EjiError.EXPECTED, parser.getPos(), "'#39'(' +
            #39'");'
          '    readArgs();'
          '    completeClassIdentifer(args);'
          '    if (!reflector.newInstance(args, result) && '
          '        !getValue(result))'
          
            '      throw new EjiError(EjiError.UNKNOWN_IDENTIFER, parser.getP' +
            'revPos(), args.identifer);'
          '    return result;'
          '  }'
          '  '
          '  /** complete identifer (args.identifer)'
          '   * dummy; see descendants'
          '   */'
          '  protected void completeClassIdentifer(Args args) {'
          '    // virtual method'
          '  }'
          '  '
          '  public void init() {'
          '    vResult = null;'
          '    ExpStackPtr = -1;'
          '    parser.init();'
          '  }'
          ''
          '  /** implementation of run(),'
          '   *  must be overloaded in descendants'
          '   */'
          '  public void execute() {'
          '    vResult = expression1();'
          '  }'
          ''
          '  /** executes source'
          '   */'
          '  public void run() {'
          
            '    if (source == null) throw new EjiError(EjiError.SOURCE_IS_NU' +
            'LL);'
          '    parser.setSource(source);'
          '    init();'
          '    parser.nextToken();'
          '    execute();'
          '  if (vResult == null)'
          '    vResult = new Variant();'
          '  }'
          '}')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 40
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlJava
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'VBScript'
      object RAHLEditor3: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          #39' I don'#39't find any normal VBScript on my hard drives'
          #39' so I put here my word basic module'
          ''
          #39' '#193#232#225#235#232#238#242#229#234#224' '#196#238#234#238#226'-'#224#226#242#238#236#224#242#232#231#224#246#232#232
          #39' '#206#225#249#232#229' '#238#225#250#229#234#242#251' '#232' '#244#243#237#234#246#232#232' '#243#239#240#224#226#235#229#237#232#255' '#196#238#234#224#236#232
          ''
          #39' '#206#225#250#229#234#242#251
          ''
          #39' Docu - '#195#235#224#226#237#251#233' '#238#225#250#229#234#242' '#239#240#232#235#238#230#229#237#232#255
          'Public ODocuApp As Docu.Application'
          #39' Database - '#225#224#231#224
          'Public ODatabase As Docu.IDatabase'
          ''
          #39' Template - '#216#224#225#235#238#237#251
          'Public OTemplate As Docu.ITemplate'
          #39' TemplateRekv - '#240#229#234#226#232#231#232#242#251' '#224#234#242#232#226#237#238#227#238' '#248#224#225#235#238#237#224
          'Public OTemplateRekv As Docu.ITemplateRekv'
          ''
          #39' Docu - '#196#238#234#243#236#229#237#242#251
          'Public ODocu As Docu.IDocu'
          #39' DocuRekv - '#240#229#234#226#232#231#232#242#251' '#224#234#242#232#226#237#238#227#238' '#228#238#234#243#236#229#237#242#224
          'Public ODocuRekv As Docu.IDocuRekv'
          ''
          #39' '#194#240#229#236#229#237#237#251#233' '#244#224#233#235' '#226' '#234#238#242#238#240#251#236' '#226#241#229' '#228#229#235#224#229#242#241#255
          'Public Const TmpFile = "C:\Temp\docuAutotest.doc"'
          #39
          #39
          #39' '#212#243#237#234#246#232#232':'
          #39
          #39' DocuCreate - '#199#224#239#243#241#234' '#196#238#234#238#226
          #39'   DocuCreate()'
          #39
          #39' DocuClose - '#199#224#234#240#251#242#232#229' '#196#238#234#238#226
          #39'   DocuClose()'
          #39
          #39' OpenTemplateNew - '#206#242#234#240#251#242#232#229' '#238#239#240#229#228#229#235#229#237#237#238#227#238' '#248#224#225#235#238#237#224' '#226' '#237#238#226#238#236' '#238#234#237#229
          #39'   OpenTemplateNew(TemplateName As String)'
          #39
          #39' ForOneTemplate - '#194#251#239#238#235#237#229#237#232#229' Macro '#228#235#255' '#248#224#225#235#238#237#224' TemplateName'
          #39'   ForOneTemplate(TemplateName As String, Macro As String)'
          #39
          #39' ForEachTemplate - '#194#251#239#238#235#237#229#237#232#229' Macro '#228#235#255' '#226#241#229#245' '#248#224#225#235#238#237#238#226
          #39'   ForEachTemplate(Macro As String)'
          #39
          ''
          'Option Explicit'
          ''
          #39' '#199#224#239#243#241#234' '#196#238#234#238#226
          'Public Sub DocuCreate()'
          '  If ODocuApp Is Nothing Then'
          '    Set ODocuApp = CreateObject("Docu.Application")'
          '    Set ODatabase = ODocuApp.Database'
          '    Set OTemplate = ODocuApp.Template'
          '    Set OTemplateRekv = ODocuApp.TemplateRekv'
          '    Set ODocu = ODocuApp.Docu'
          '    Set ODocuRekv = ODocuApp.DocuRekv'
          '  End If'
          'End Sub'
          ''
          #39' '#199#224#234#240#251#242#232#229' '#196#238#234#238#226
          'Public Sub DocuClose()'
          '  Set ODatabase = Nothing'
          '  Set OTemplate = Nothing'
          '  Set OTemplateRekv = Nothing'
          '  Set ODocu = Nothing'
          '  Set ODocuRekv = Nothing'
          '  Set ODocuApp = Nothing'
          'End Sub'
          ''
          #39' '#206#242#234#240#251#242#232#229' '#238#239#240#229#228#229#235#229#237#237#238#227#238' '#248#224#225#235#238#237#224' '#226' '#237#238#226#238#236' '#238#234#237#229
          'Private Sub OpenTemplateNew(TemplateName As String)'
          '  ODatabase.Active = True'
          '  ODocuApp.Mode = duTemplate'
          '  If OTemplate.Locate("Name", TemplateName) Then'
          '    OTemplate.SyncTree'
          '    OTemplate.SaveText (TmpFile)'
          '    Documents.Open (TmpFile)'
          '    ActiveDocument.Select'
          '    Selection.Copy'
          '    ActiveDocument.Close'
          '    Documents.Add'
          '    Selection.Paste'
          '  End If'
          'End Sub'
          ''
          #39' '#194#251#239#238#235#237#229#237#232#229' Macro '#228#235#255' '#248#224#225#235#238#237#224' TemplateName'
          'Public Sub ForOneTemplate(TemplateName As String)'
          'Dim Abort As Boolean'
          '  Abort = False'
          '  ODatabase.Active = True'
          '  ODocuApp.Mode = duTemplate'
          '  If OTemplate.Locate("Name", TemplateName) Then'
          '    OTemplate.SyncTree'
          '    OTemplate.SaveText (TmpFile)'
          '    Documents.Open (TmpFile)'
          '    If Main.TemplateMacro(100, Abort) And Not Abort Then'
          '      ActiveDocument.Save'
          '      ActiveDocument.Close'
          '      OTemplate.LoadText (TmpFile)'
          '    Else'
          '      ActiveDocument.Close'
          '    End If'
          '    If Abort Then'
          '      Exit Sub'
          '    End If'
          '  End If'
          'End Sub'
          ''
          #39' '#194#251#239#238#235#237#229#237#232#229' Macro '#228#235#255' '#226#241#229#245' '#248#224#225#235#238#237#238#226
          'Public Sub ForEachTemplate()'
          'Dim Abort As Boolean'
          'Dim Con, Cur As Integer'
          '  Abort = False'
          '  ODatabase.Active = True'
          '  ODocuApp.Mode = duTemplate'
          '  OTemplate.First'
          '  Cur = 0'
          '  Con = OTemplate.Count'
          '  While Not OTemplate.EOF'
          '    If (OTemplate.FieldValues("Icon") > 2) And _'
          '       (OTemplate.FieldValues("Uni") > 0) Then'
          '      OTemplate.SaveText (TmpFile)'
          '      Documents.Open (TmpFile)'
          
            '      If Main.TemplateMacro(Cur / Con * 100, Abort) And Not Abor' +
            't Then'
          '        ActiveDocument.Save'
          '        ActiveDocument.Close'
          '        OTemplate.LoadText (TmpFile)'
          '      Else'
          '        ActiveDocument.Close'
          '      End If'
          '      If Abort Then'
          '        Exit Sub'
          '      End If'
          '    End If'
          '    Cur = Cur + 1'
          '    OTemplate.Next'
          '  Wend'
          'End Sub'
          ''
          #39' '#194#251#239#238#235#237#229#237#232#229' Macro '#228#235#255' '#226#241#229#245' '#228#238#234#243#236#229#237#242#238#226
          'Public Sub ForEachDocu()'
          'Dim Abort As Boolean'
          'Dim Con, Cur As Integer'
          'Dim F As Variant'
          '  Abort = False'
          '  ODatabase.Active = True'
          '  ODocuApp.Mode = duDocu'
          '  ODocu.Unlink'
          '  ODocu.First'
          '  Cur = 0'
          '  Con = ODocu.Count'
          '  While Not ODocu.EOF'
          '    F = ODocu.FieldValues("Flags")'
          '    If VarType(F) = vbNull Then'
          '      F = 0'
          '    End If'
          '    If (ODocu.FieldValues("Uni") > 0) And _'
          '       ((F And duPassword) = 0) And _'
          '       ((F And duReadOnly) = 0) And _'
          '       (ODocu.FieldValues("Typ") = duWord) Then'
          '      ODocu.SaveText (TmpFile)'
          '      Documents.Open (TmpFile)'
          
            '      If Main.TemplateMacro(Cur / Con * 100, Abort) And Not Abor' +
            't Then'
          '        ActiveDocument.Save'
          '        ActiveDocument.Close'
          '        ODocu.LoadText (TmpFile)'
          '      Else'
          '        ActiveDocument.Close'
          '      End If'
          '      If Abort Then'
          '        Exit Sub'
          '      End If'
          '    End If'
          '    Cur = Cur + 1'
          '    ODocu.Next'
          '  Wend'
          '  ODocu.Link'
          'End Sub'
          '')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 40
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlVB
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'HTML'
      object RAHLEditor4: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          '<html>'
          ''
          '<head>'
          '<meta name="GENERATOR" content="Microsoft FrontPage 3.0">'
          '<title>JVCLmp;A Library home page</title>'
          '</head>'
          '<unknowntag unknownattr="value">'
          '<!-- unknown tags are highlighted with "statement" color,'
          '  this usefull for xml-files -->'
          
            '<body background="zertxtr.gif" bgcolor="#000000" text="#FFFFFF" ' +
            'link="#FF0000"'
          'alink="#FFFF00">'
          ''
          '<table border="0" width="100%">'
          '  <tr>'
          
            '    <td width="33%" valign="top" align="left"><font face="Arial"' +
            ' color="#008080"><u><strong><small><small>Official</small></smal' +
            'l><br>'
          '    <small><small>Home </small></small><br>'
          '    <small><small>Page</small></small></strong></u></font></td>'
          
            '    <td width="33%"><p align="center"><big><font color="#00FF00"' +
            '><big><strong>JVCLmp;A Library</strong>'
          '    <big><a href="racdplay.zip"><br>'
          
            '    </a></big></big></font></big><font face="Arial" color="#0080' +
            '80"><small><small>version 1.10</small></small><br>'
          '    <small><small>11 Feb 1999</small></small></font></td>'
          '    <td width="34%"></td>'
          '  </tr>'
          '</table>'
          ''
          
            '<p align="left">Download last JVCLmp;A Library version now - <fo' +
            'nt face="Arial"'
          
            'color="#00FFFF"><a href="http://www.torry.ru/vcl/packs/ralib.zip' +
            '"><small>ralib110.zip</small></a>'
          
            '</font><font face="Arial" color="#008080"><small><small>(575 Kb)' +
            '</small></small></font>.</p>'
          ''
          '<p align="left"><u><font color="#00FF00">Hot News</font><br>'
          '</u><font color="#FFFF80">11 Feb 1999</font><br>'
          'Version 1.1 released. <br>'
          
            'New components added. The main work pointed to new interpreter -' +
            ' JVCLmp; Interpreter 2 -'
          'JvI2.<br>'
          'JvI2 is very cool, I think. </p>'
          ''
          '<p align="left"><font color="#00FF00"><u>Overview</u></font></p>'
          ''
          
            '<p align="left">JVCLmp;A Library contains a number of components' +
            ', classes and routines for'
          
            'Borland Delphi with full source code. This library is compatible' +
            ' with Borland Delphi 2, 3,'
          
            '4 and Borland C++ Builder 1, 3. Read Compatibility section for m' +
            'ore information.</p>'
          ''
          
            '<p align="left">JVCLmp;A Library is a freeware product. Feel fre' +
            'e to distribute the library'
          'as long as all files are unmodified and kept together.<br>'
          
            'The authors disclaim all warranties as to this software, whether' +
            ' express or implied,'
          
            'including without limitation any implied warranties of merchanta' +
            'bility or fitness for a'
          
            'particular purpose. Use under your own responsibility, but comme' +
            'nts (even critique) in'
          'English (or in Russian) are welcome.</p>'
          ''
          
            '<p align="left"><u><font color="#00FF00">Compatibility</font></u' +
            '></p>'
          ''
          
            '<p align="left">This version of JVCLmp;A Library is writen and f' +
            'ully tested with Borland'
          'Delphi 3 Client/Server edition.<br>'
          
            'JVCLmp;A Library was some tested on Borland Delphi 2 Developer E' +
            'dition, Borland Delphi 4'
          
            'Client/Server edition, Borland C++ Builder 1.0 Client/Server Edi' +
            'tion and Borland C++'
          'Builder 3.0 Client/Server Edition.<br>'
          
            'Some components don'#39't works with all versions of&nbsp; Delphi an' +
            'd Builder (see notes for'
          'components).<br>'
          
            'JVCLmp;A Library is not tested properly with this products.Use a' +
            't your own risk.</p>'
          ''
          '</body>'
          '</html>'
          '')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 40
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlHtml
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clGreen
        Colors.Identifier.ForeColor = clRed
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
        Colors.Statement.ForeColor = clLime
      end
    end
    object TabSheet10: TTabSheet
      Caption = 'Ini'
      object RAHLEditor5: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          '; for 16-bit app support'
          '[fonts]'
          'plain text'
          '[extensions]'
          '[mci extensions]'
          '[files]'
          'PROMT Helv WE 8,10 (VGA res)=prmhelvw.fon'
          'PROMT Helv Cyr 8,10,12,14,18,24 (VGA res)=prmhelvr.fon'
          '[Mail]'
          'MAPI=1'
          'CMC=1'
          'CMCDLLNAME=mapi.dll'
          'CMCDLLNAME32=mapi32.dll'
          'MAPIX=1'
          'MAPIXVER=1.0.0.1'
          'OLEMessaging=1'
          '[MCI Extensions.BAK]'
          'asf=MPEGVideo'
          'asx=MPEGVideo'
          'ivf=MPEGVideo'
          'm3u=MPEGVideo'
          'mp2v=MPEGVideo'
          'mp3=MPEGVideo'
          'mpv2=MPEGVideo'
          'wax=MPEGVideo'
          'wm=MPEGVideo'
          'wma=MPEGVideo'
          'wmv=MPEGVideo'
          'wvx=MPEGVideo'
          '[MSUCE]'
          'Advanced=1'
          'CodePage=Windows: Cyrillic'
          'Font=Tahoma')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 40
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlIni
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clGreen
        Colors.Identifier.ForeColor = clGreen
        Colors.Preproc.ForeColor = clGreen
        Colors.Declaration.Style = [fsBold]
        Colors.Declaration.ForeColor = clNavy
        Colors.Statement.Style = [fsBold]
        Colors.PlainText.ForeColor = clGray
      end
    end
    object TabSheet11: TTabSheet
      Caption = 'Perl'
      object RAHLEditor6: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          '#!/usr/local/bin/perl'
          '#'
          '# scores.pl'
          '# created:       2000/10/05           '
          '# last modified: 2000/10/06'
          '#'
          '# '#210#224#225#235#232#246#224' '#240#229#234#238#240#228#238#226
          '#'
          '# In the event of a failure, anything written to stderr can'
          '# be found in the "tmp" directory.'
          ''
          '#'
          '# '#212#238#240#236#224#242' '#226#240#229#236#229#237#232': "yyyy-MM-dd HH:mm:ss.S";'
          ''
          '# '#239#243#242#252', '#227#228#229' '#245#240#224#237#255#242#241#255' '#244#224#233#235#251' '#241' '#242#224#225#235#232#246#224#236#232' '#240#229#234#238#240#228#238#226
          '$DATAPATH = "./scores-data/";'
          '# '#236#224#234#241#232#236#224#235#252#237#238#229' '#234#238#235#232#247#229#241#242#226#238' '#232#227#240#238#234#238#226' '#226' '#244#224#233#235#229
          '$MAX_PLAYERS = 10;'
          ''
          '#require '#39'cgi.pl'#39';'
          ''
          '# '#247#232#242#224#229#236' '#239#224#240#224#236#229#242#240#251
          'if ($ENV{REQUEST_METHOD} eq "GET") {'
          '  parse_query_string();'
          '} elsif ($ENV{REQUEST_METHOD} eq "POST") {'
          '  parse_post_data();'
          '} else {'
          '  method_not_supported_message();'
          '}'
          ''
          '# '#239#240#238#226#229#240#255#229#236' '#239#224#240#224#236#229#242#240#251
          'missing_filename_message() unless $ARGS{filename};'
          ''
          '$filename = "$ARGS{'#39'filename'#39'}.dat";'
          ''
          'chdir $DATAPATH;'
          ''
          '# '#239#240#238#226#229#240#255#229#236' '#237#224#235#232#247#232#229' '#244#224#233#235#224', '#241#238#231#228#224#184#236' '#237#238#226#251#233' '#239#240#232' '#237#229#238#225#245#238#228#232#236#238#241#242#232
          'check_file_exists();'
          ''
          '# '#247#232#242#224#229#236' '#244#224#233#235
          'read_data();'
          ''
          '# '#239#229#240#229#228#224#237' '#239#224#240#224#236#229#242#240' player, '#231#237#224#247#232#242' '#228#238#225#224#226#235#255#229#236' '#229#227#238' '#226' '#242#224#225#235#232#246#243
          '# '#240#229#234#238#240#228#238#226', '#229#241#235#232' '#239#238#239#224#228#224#229#242', '#232' '#241#238#245#240#224#237#255#229#236' '#237#238#226#251#233' '#244#224#233#235
          'if ($ARGS{player}) {'
          '  add_player();'
          '  write_data();'
          '}'
          ''
          '# Response message.'
          ''
          '#response_html();'
          'response_xml();'
          ''
          ''
          'EXIT;'
          ''
          ''
          'sub parse_query_string {'
          '  (@args) = split(/&/, $ENV{QUERY_STRING});'
          '  foreach $arg (@args) {'
          '      ($arg, $value) = split(/=/, $arg);'
          '      $value =~ tr/+/ /;'
          '      $value =~ s/%([\dA-Fa-f][\dA-Fa-f])/pack("C", hex($1))/eg;'
          
            '      if (($arg eq '#39'query'#39') && ($value ne '#39#39')) { $ARGS{$arg} = '#39 +
            '.*'#39' }'
          '      else { $ARGS{$arg} = $value }'
          '  }'
          '}'
          ''
          'sub parse_post_data {'
          '  # Get the input'
          '  read(STDIN, $data, $ENV{CONTENT_LENGTH});'
          '  '
          '  # Split the name-value pairs'
          '  @pairs = split(/&/, $data);'
          '  '
          '  foreach $pair (@pairs) {'
          '    ($name, $value) = split(/=/, $pair);'
          '  '
          '    # Convert the HTML encoding'
          '    $value =~ tr/+/ /;'
          '    $value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;'
          '    $value =~ s/<!--(.|\n)*-->//g;'
          '  '
          '    # Convert HTML stuff as necessary.'
          '    $value =~ s/<([^>]|\n)*>//g;'
          '  '
          '    $ARGS{$name} = $value;'
          '  }'
          '}'
          ''
          '# '#244#238#240#236#232#240#243#229#242' '#241#238#238#225#249#229#237#232#229' '#238#225' '#238#248#232#225#234#229
          'sub missing_filename_message {'
          '  print "Content-type: text/html\n\n";'
          
            '  print "<HTML><HEAD><TITLE>Missing filename</TITLE></HEAD><BODY' +
            '>\n";'
          '  print "<H1>Error</H1>\n";'
          '  print "<H3>\"filename\" parameter is missing</H3>\n";'
          '  print "</BODY></HTML>\n";'
          ''
          '  exit;'
          '}'
          ''
          'sub response_html {'
          '  print "Content-type: text/html\n\n";'
          '  print "<HTML>\n";'
          '  print "<HEAD><TITLE>$game scores</TITLE></HEAD>\n";'
          '  print "<BODY bgcolor=white>\n";'
          '  print "<H1>$game scores</H1>\n";'
          '  '
          '  foreach $player (@players) {'
          
            '    print "$player->{name} - $player->{score} - $player->{time}<' +
            'BR>\n";'
          '  }'
          '  '
          '  print <<END;'
          '  <FORM METHOD=POST>'
          '    <INPUT TYPE=HIDDEN NAME="filename" VALUE=$ARGS{filename}>'
          '    NAME: <INPUT TYPE=TEXT NAME="player">'
          '    SCORE: <INPUT TYPE=TEXT NAME="score">'
          '    <INPUT TYPE=SUBMIT VALUE="Add Score">'
          '  </FORM>'
          'END'
          '  print "</BODY>\n";'
          '  print "</HTML>\n";'
          '}'
          ''
          'sub response_xml {'
          '  ## ???'
          '  print "Content-type: text/xml\n\n";'
          '  print "<?xml version='#39'1.0'#39' encoding='#39'windows-1251'#39' ?>\n";'
          '  print "<scores>\n";'
          '  '
          '  foreach $player (@players) {'
          '    print "  <player>\n";'
          '    print "    <player-name>$player->{name}</player-name>\n";'
          '    print "    <player-score>$player->{score}</player-score>\n";'
          '    print "    <player-time>$player->{time}</player-time>\n";'
          
            '    print "    <!-- \"player-date\" tag is deprecated; use \"pla' +
            'yer-time\" tag instead -->\n";'
          '    print "    <player-date>$player->{time}</player-date>\n";'
          '    print "  </player>\n";'
          '  }'
          ''
          '  print "</scores>";'
          '}'
          ''
          'sub file_not_exists_message {'
          '  print "Content-type: text/html\n\n";'
          
            '  print "<HTML><HEAD><TITLE>File not exists</TITLE></HEAD><BODY>' +
            '\n";'
          '  print "<H1>Error</H1>\n";'
          '  print "<H3>File \"$ARGS{filename}\" does not exists</H3>\n";'
          '  print "</BODY></HTML>\n";'
          ''
          '  exit;'
          '}'
          ''
          'sub method_not_supported_message {'
          '  print "Content-type: text/html\n\n";'
          
            '  print "<HTML><HEAD><TITLE>Method not supported</TITLE></HEAD><' +
            'BODY>\n";'
          '  print "<H1>Error</H1>\n";'
          '  print "<H3>Method not supported</H3>\n";'
          '  print "</BODY></HTML>\n";'
          ''
          '  exit;'
          '}'
          ''
          '# '#239#240#238#226#229#240#255#229#236' '#237#224#235#232#247#232#229' '#244#224#233#235#224', '#241#238#231#228#224#184#236' '#237#238#226#251#233' '#239#240#232' '#237#229#238#225#245#238#228#232#236#238#241#242#232
          'sub check_file_exists {'
          '  if (!(-e $filename)) {'
          ''
          '    #'#241#238#231#228#224#184#236' '#239#238#228#234#224#242#224#235#238#227#232
          '    @path = split('#39'/'#39', $filename);'
          '    $filename = pop(@path);'
          '    foreach $dir (@path) {'
          '      mkdir $dir, 0755;'
          '      chdir $dir;'
          '    }'
          '  '
          '    open FILE, ">$filename";'
          '    #print FILE "new game\n";'
          '    close FILE;'
          '  } else {'
          ''
          '    # '#231#224#245#238#228#232#236' '#226' '#239#238#228#234#224#242#224#235#238#227
          '    #@path = split('#39'/'#39', $filename);'
          '    #$filename = pop(@path);'
          '    #foreach $dir (@path) {'
          '    #  chdir $dir;'
          '    #}'
          '  }'
          '}'
          ''
          '# '#247#232#242#224#229#242' '#242#224#225#235#232#246#243' '#240#229#234#238#240#228#238#226' '#232#231' '#244#224#233#235#224
          'sub read_data {'
          '  open (FILE, "$filename") || die "Can'#39't open $filename: $!\n";'
          '  @LINES = <FILE>;'
          '  close (FILE);'
          '  chomp @LINES;'
          ''
          '  # '#241#247#232#242#251#226#224#229#236' '#232#227#240#238#234#238#226
          '  $offset = 0;'
          '  for ($i = 0; $i < $MAX_PLAYERS; $i++) {'
          '    $player = @LINES[$offset + $i];'
          '    ($name, $score, $time) = split(/=/, $player);'
          '    last if !$score;'
          '    push @players, {(name=>$name, score=>$score, time=>$time)};'
          '  }'
          '}'
          ''
          '# '#231#224#239#232#241#251#226#224#229#242' '#242#224#225#235#232#246#243' '#240#229#234#238#240#228#238#226' '#226' '#244#224#233#235
          'sub write_data {'
          '  open (FILE, ">$filename") || die "Can'#39't open $filename: $!\n";'
          '  foreach $player (@players) {'
          
            '    print FILE "$player->{name}=$player->{score}=$player->{time}' +
            '\n";'
          '  }'
          '  close (FILE);'
          '}'
          ''
          '# '#228#238#225#224#226#235#255#229#242' '#232#227#240#238#234#224' '#226' '#242#224#225#235#232#246#243' '#240#229#234#238#240#228#238#226' '#226' '#237#243#230#237#243#254' '#239#238#231#232#246#232#254
          '# '#232#235#232' '#237#229' '#228#238#225#224#226#235#255#229#242
          'sub add_player {'
          '  $name = $ARGS{player};'
          '  $score = $ARGS{score};'
          '  $time = formatCurrentTime();'
          '  $pos = -1;'
          '  for ($i = 0; $i <= $#players; $i ++) {'
          '    $player = $players[$i];'
          '    if ($player->{score} > $score) {'
          '      $pos = $i;'
          '      last;'
          '    }'
          '  }'
          '  # '#229#241#235#232' '#239#238#239#224#235' '#226' '#242#224#225#235#232#246#243' '#240#229#234#238#240#228#238#226
          '  if ($pos > -1) {'
          '    # '#241#236#229#241#242#232#242#252' '#226#237#232#231' '#226#241#229#245' '#241' '#245#243#228#248#232#236' '#240#229#231#243#235#252#242#224#242#238#236
          '    for ($i = $#players; $i >= $pos; $i --) {'
          '      $players[$i + 1] = $players[$i];'
          '    }'
          
            '    $players[$pos] = {(name=>$name, score=>$score, time=>$time)}' +
            ';'
          '    if ($#players >= $MAX_PLAYERS) {'
          '      $#players = $MAX_PLAYERS - 1'
          '    }'
          '  } elsif ($#players < $MAX_PLAYERS - 1) {'
          '    push @players, {(name=>$name, score=>$score, time=>$time)};'
          '  }'
          '}'
          ''
          'sub formatCurrentTime {'
          '  # '#212#238#240#236#224#242' '#226#240#229#236#229#237#232': "yyyy-MM-dd HH:mm:ss.S";'
          '  ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday) = gmtime(time);'
          '  $year += 1900;'
          '  $mon = sprintf("%02d", $mon + 1);'
          '  $mday = sprintf("%02d", $mday);'
          '  $hour = sprintf("%02d", $hour);'
          '  $min = sprintf("%02d", $min);'
          '  $sec = sprintf("%02d", $sec);'
          '  return "$year-$mon-$mday $hour:$min:$sec.0";'
          '}')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 40
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlPerl
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clGreen
        Colors.Identifier.ForeColor = clGreen
        Colors.Preproc.ForeColor = clGreen
        Colors.Declaration.Style = [fsBold]
        Colors.Declaration.ForeColor = clNavy
        Colors.Statement.Style = [fsBold]
        Colors.PlainText.ForeColor = clGray
      end
    end
    object TabSheet12: TTabSheet
      Caption = 'PHP'
      ImageIndex = 11
      object RAHLEditor7: TJvHLEditor
        Tag = 2
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          'print  echo("Hello, World!"); // comment')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 40
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.Enabled = True
        Completion.ItemHeight = 13
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlPhp
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clBlack
        Colors.Declaration.Style = [fsBold]
        Colors.Declaration.ForeColor = clNavy
        Colors.Statement.Style = [fsBold]
        Colors.PlainText.ForeColor = clGray
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Text'
      object RAEditor3: TJvHLEditor
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        Lines.Strings = (
          'JVCL Library Software License Agreement'
          ''
          'BEFORE PROCEEDING WITH THE INSTALLATION '
          'AND/OR USE OF THIS SOFTWARE, CAREFULLY'
          'READ THE FOLLOWING TERMS AND CONDITIONS'
          'OF THIS LICENSE AGREEMENT AND LIMITED'
          'WARRANTY (The "Agreement").'
          ''
          'BY INSTALLING OR USING THIS SOFTWARE YOU'
          'INDICATE YOUR ACCEPTANCE OF THIS'
          'AGREEMENT. IF YOU DO NOT ACCEPT OR AGREE'
          'WITH THESE TERMS, YOU MAY NOT INSTALL OR'
          'USE THIS SOFTWARE!'
          ''
          'PREAMBLE'
          ''
          'The terms and conditions of the JVCL Library'
          'Software License Agreement have one major'
          'goal in mind; to foster a development '
          'community based around the JVCL components'
          'library and associated source code.'
          'JVCL Library does however reserve the right'
          'as the sole distributor of the library '
          'source code. Hence although we encourage'
          'you to change and modify the library to '
          'suit your needs, you may not distribute'
          'derivative works based on the library'
          'source code without express written '
          'permission from JVCL Library authors. '
          'Worthwhile changes and modifications to '
          'the libraries may be submitted to JVCL '
          'Library authors for integration into a'
          'future release of the product.'
          ''
          'LICENSE'
          ''
          'This software, including documentation, '
          'source code, object code and/or additional'
          'materials (the "Software") is owned by '
          'RALib authors.'
          'This Agreement does not provide you with'
          'title or ownership of Product, but only a'
          'right of limited use as outlined in this'
          'license agreement. JVCL Library authors'
          'hereby grant you a non-exclusive, '
          'royalty free license to use the Software'
          'as set forth below:'
          ''
          ' . integrate the Software with your'
          '   Applications, subject to the'
          '   redistribution terms below.'
          ' . modify or adapt the Software in whole'
          '   or in part for the development of '
          '   Applications based on the Software.'
          ' . use portions of the JVCL Library source'
          '   code or JVCL Library Demo Programs in'
          '   your own products and libraries.'
          ''
          'REDISTRIBUTION RIGHTS'
          ''
          'You are granted a non-exclusive, '
          'royalty-free right to reproduce and'
          'redistribute executable files created'
          'using the Software (the "Executable Code")'
          'in conjunction with software products that'
          'you develop and/or market (the '
          '"Applications"). '
          ''
          'RESTRICTIONS'
          ''
          'Without the expressed, written consent of'
          'JVCL Library authors, you may NOT:'
          ''
          ' . distribute modified versions of the '
          '   Software, in whole or in part.'
          ' . rent or lease the Software.'
          ' . sell any portion of the Software on '
          '   its own, without integrating it into '
          '   your Applications as Executable Code.'
          ''
          'SELECTION AND USE'
          ''
          'You assume full responsibility for the'
          'selection of the Software to achieve your'
          'intended results and for the installation,'
          'use and results obtained from the Software.'
          ''
          'LIMITED WARRANTY'
          ''
          'THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT'
          'WARRANTY OF ANY KIND EITHER EXPRESSED OR'
          'IMPLIED, INCLUDING BUT NOT LIMITED TO THE'
          'IMPLIED WARRANTIES MERCHANTIBILITY AND'
          'FITNESS FOR A PARTICULAR PURPOSE. THE '
          'ENTIRE RISK AS TO THE QUALITY AND '
          'PERFORMANCE OF THE PRODUCT IS WITH YOU. '
          'SHOULD THE PRODUCT PROVE DEFECTIVE, YOU'
          'ASSUME THE COST OF ALL NECESSARY SERVICING'
          'OR ERROR CORRECTION.'
          ''
          'JVCL LIBRARY AUTHORS DO NOT WARRANT THAT'
          'THE FUNCTIONS CONTAINED IN THE SOFTWARE '
          'WILL MEET YOUR REQUIREMENTS OR THAT THE '
          'OPERATION OF THE SOFTWARE WILL BE '
          'UNINTERRUPTED OR ERROR FREE.'
          ''
          'No oral or written information given by '
          'JVCL Library shall create a warranty.'
          ''
          'LIMITATION OF REMEDIES AND LIABILITY.'
          ''
          'IN NO EVENT SHALL JVCL LIBRARY AUTHORS, OR'
          'ANY OTHER PARTY WHO MAY HAVE DISTRIBUTED'
          'THE SOFTWARE AS PERMITTED ABOVE, BE LIABLE'
          'FOR DAMAGES, INCLUDING ANY GENERAL, '
          'SPECIAL, INCIDENTAL, OR CONSEQUENTIAL '
          'DAMAGES ARISING OUT OF THE USE OR '
          'INABILITY TO USE THE SOFTWARE (INCLUDING'
          'BUT NOT LIMITED TO LOSS OF DATA OR DATA '
          'BEING RENDERED INACCURATE OR LOSSES '
          'SUSTAINED BY YOU OR THIRD PARTIES OR '
          'FAILURE OF THE SOFTWARE TO OPERATE WITH '
          'ANY OTHER PRODUCTS), EVEN IF SUCH HOLDER '
          'OR OTHER PARTY HAS BEEN ADVISED OF THE '
          'POSSIBILITY OF SUCH DAMAGES.'
          ''
          'If you have any questions regarding this '
          'agreement, please contact JVCL Library'
          'authors at their e-mail addresses:'
          '  blacknbs@chat.ru')
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 0
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.ItemHeight = 16
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
        Highlighter = hlNone
        Colors.Comment.Style = [fsItalic]
        Colors.Comment.ForeColor = clOlive
        Colors.Number.ForeColor = clNavy
        Colors.Strings.ForeColor = clPurple
        Colors.Symbol.ForeColor = clBlue
        Colors.Reserved.Style = [fsBold]
        Colors.Reserved.ForeColor = clBlack
        Colors.Identifier.ForeColor = clBlack
        Colors.Preproc.ForeColor = clGreen
        Colors.Statement.Style = [fsBold]
        OnReservedWord = RAEditor3ReservedWord
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Empty'
      object RAEditor4: TJvEditor
        Left = 0
        Top = 0
        Width = 533
        Height = 318
        Cursor = crIBeam
        GutterWidth = 16
        RightMarginVisible = False
        RightMargin = 0
        RightMarginColor = clSilver
        Completion.DropDownCount = 8
        Completion.ItemHeight = 16
        Completion.Interval = 800
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        SelForeColor = clHighlightText
        SelBackColor = clHighlight
        OnPaintGutter = RAEditorPaintGutter
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabStop = True
        UseDockManager = False
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 541
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 333
      Height = 13
      Caption = 
        'You can edit text. Press Ctrl+J and Ctrl+Space to see code compl' +
        'etion.'
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 379
    Width = 541
    Height = 19
    Panels = <
      item
        Width = 60
      end
      item
        Width = 80
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object GutterImages: TImageList
    DrawingStyle = dsTransparent
    Height = 15
    Width = 12
    Left = 40
    Top = 232
    Bitmap = {
      494C01010A000E0004000C000F00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000003C0000000100200000000000002D
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400000084
      0000848400000084000084840000008400008484000000000000000000000000
      000000000000C6C6C60084840000008400008484000000840000848400000084
      0000848400000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600008400008484
      0000000000000000000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400000000
      0000008400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000840000848400000000
      0000848400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400000000
      0000008400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400000000000000000000848400000000
      0000848400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400008484
      0000000000000000000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000000000000084000084840000000000000000
      0000008400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000000000008484000000840000848400000000
      0000848400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000000000000084000084840000008400000000
      0000008400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400000084
      0000000000000000000000000000008400008484000000000000848484000000
      000000000000C6C6C60084840000008400000000000000000000000000000084
      0000848400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600008400008484
      0000008400008484000000840000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400008484
      0000008400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600848484000000
      00000000000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400000084
      0000848400000084000084840000008400008484000000000000000000000000
      000000000000C6C6C60084840000008400008484000000840000848400000084
      00008484000000000000000000000000000000000000C6C6C600848400000084
      0000848400000084000084840000008400008484000000000000000000000000
      000000000000C6C6C60084840000008400008484000000840000848400000084
      00008484000000000000000000000000000000000000C6C6C600008400008484
      0000008400008484000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000000000000000000000000008484
      00000084000000000000848484000000000000000000C6C6C600008400008484
      0000000000000000000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000000000000008400008484
      00000084000000000000848484000000000000000000C6C6C600848400000084
      0000848400000084000000000000008400008484000000000000848484000000
      000000000000C6C6C60084840000000000008484000000840000848400000000
      00008484000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000000000848400000084
      00008484000000000000848484000000000000000000C6C6C600008400008484
      0000008400008484000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400000000
      00000084000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000000000000008400008484
      00000084000000000000848484000000000000000000C6C6C600848400000000
      0000000000000000000000000000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000840000848400000000
      00008484000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000840000000000000084
      00008484000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000000000000000000000000000000000008484
      00000084000000000000848484000000000000000000C6C6C600008400000000
      0000000000000000000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000000000008484
      00000084000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000000000000008400008484000000000000848484000000
      000000000000C6C6C60084840000000000008484000000840000848400000084
      00008484000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000084840000008400008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000840000848400000000
      00008484000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000000000000084000084840000008400008484
      00000084000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000000000000084000084840000008400000000
      00000084000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000000000000008400008484000000000000848484000000
      000000000000C6C6C60084840000000000000000000000000000000000000000
      00008484000000000000848484000000000000000000C6C6C600848400000084
      0000000000000000000000000000008400008484000000000000848484000000
      000000000000C6C6C60084840000000000000000000000000000000000000000
      00008484000000000000848484000000000000000000C6C6C600008400008484
      0000008400008484000000840000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400008484
      00000084000000000000848484000000000000000000C6C6C600008400008484
      0000008400008484000000840000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400008484
      0000008400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600848484000000
      00000000000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084848400000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600848484000000
      00000000000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600848400000084
      0000848400000084000084840000008400008484000000000000000000000000
      000000000000C6C6C60084840000008400008484000000840000848400000084
      00008484000000000000000000000000000000000000C6C6C600848400000084
      0000848400000084000084840000008400008484000000000000000000000000
      000000000000C6C6C60084840000008400008484000000840000848400000084
      00008484000000000000000000000000000000000000C6C6C600008400008484
      0000000000000000000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000000000000000000000000000000000000000
      00000084000000000000848484000000000000000000C6C6C600008400000000
      0000000000000000000000000000000000000084000000000000848484000000
      000000000000C6C6C60000840000848400000000000000000000000000008484
      00000084000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000000000848400000084
      00008484000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000084840000008400008484000000000000848484000000
      000000000000C6C6C60084840000000000008484000000840000848400000000
      00008484000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000000000000008400008484
      00000084000000000000848484000000000000000000C6C6C600008400008484
      0000000000008484000000840000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400000000
      00000084000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000000000848400000084
      00008484000000000000848484000000000000000000C6C6C600848400000084
      0000848400000000000084840000008400008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000840000848400000000
      00008484000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000000000000008400008484
      00000084000000000000848484000000000000000000C6C6C600008400008484
      0000008400008484000000000000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000000000000000000008484
      00000084000000000000848484000000000000000000C6C6C600848400000000
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000000000008484000000000000848400000084
      00008484000000000000848484000000000000000000C6C6C600848400000084
      0000848400000084000084840000000000008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000840000848400000000
      00008484000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000848400000000000000000000008400008484
      00000084000000000000848484000000000000000000C6C6C600008400000000
      0000008400008484000000840000000000000084000000000000848484000000
      000000000000C6C6C60000840000000000000084000084840000008400000000
      00000084000000000000848484000000000000000000C6C6C600848400000084
      0000000000000000000000000000008400008484000000000000848484000000
      000000000000C6C6C60084840000008400008484000000000000848400000084
      00008484000000000000848484000000000000000000C6C6C600848400000084
      0000000000000000000000000000008400008484000000000000848484000000
      000000000000C6C6C60084840000008400000000000000000000000000000084
      00008484000000000000848484000000000000000000C6C6C600008400008484
      0000008400008484000000840000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400008484
      00000084000000000000848484000000000000000000C6C6C600008400008484
      0000008400008484000000840000848400000084000000000000848484000000
      000000000000C6C6C60000840000848400000084000084840000008400008484
      0000008400000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600848484000000
      00000000000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084848400000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600848484000000
      00000000000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000300000003C0000000100010000000000E00100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFFFF000000000000300300000000000010010000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000008008000000000000C01C010000000000
      FFFFFFFFFFFF0000003003003003000000100100100100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000008008008008000000C01C01C01C010000FFFFFFFFFFFF0000
      0030030030030000001001001001000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8008008008000000C01C01C01C01000000000000000000000000000000000000
      000000000000}
  end
  object ilCompletions: TImageList
    DrawingStyle = dsTransparent
    Left = 40
    Top = 168
  end
  object RegAuto1: TJvFormStorage
    OnSavePlacement = RegAuto1AfterSave
    OnRestorePlacement = RegAuto1AfterLoad
    StoredValues = <>
    Left = 40
    Top = 288
  end
end
