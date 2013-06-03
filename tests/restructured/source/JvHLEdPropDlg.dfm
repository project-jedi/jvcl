object RAHLEditorParamsForm: TJvHLEditorParamsForm
  Left = 147
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Editor Properties'
  ClientHeight = 369
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 8
    Top = 8
    Width = 419
    Height = 321
    ActivePage = tsEditor
    TabOrder = 0
    object tsEditor: TTabSheet
      Caption = 'Editor'
      object lblEditorSpeedSettings: TLabel
        Left = 30
        Top = 11
        Width = 99
        Height = 13
        Alignment = taRightJustify
        Caption = 'Editor SpeedSettings'
      end
      object lblTabStops: TLabel
        Left = 66
        Top = 230
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = '&Tab stops:'
      end
      object cbKeyboardLayot: TComboBox
        Left = 136
        Top = 6
        Width = 267
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Default keymapping')
      end
      object gbEditor: TGroupBox
        Left = 8
        Top = 35
        Width = 395
        Height = 150
        Caption = 'Editor options:'
        TabOrder = 1
        object cbUndoAfterSave: TCheckBox
          Left = 184
          Top = 16
          Width = 200
          Height = 17
          Caption = '&Undo after sa&ve'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = NotImplemented
        end
        object cbDoubleClickLine: TCheckBox
          Left = 184
          Top = 56
          Width = 200
          Height = 17
          Caption = '&Double click line'
          TabOrder = 1
        end
        object cbKeepTrailingBlanks: TCheckBox
          Left = 184
          Top = 36
          Width = 200
          Height = 17
          Caption = '&Keep trailing blanks'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbSytaxHighlighting: TCheckBox
          Left = 184
          Top = 76
          Width = 200
          Height = 17
          Caption = 'Use &syntax highlight'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object cbAutoIndent: TCheckBox
          Left = 8
          Top = 16
          Width = 169
          Height = 17
          Caption = '&Auto indent mode'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
        object cbSmartTab: TCheckBox
          Left = 8
          Top = 36
          Width = 169
          Height = 17
          Caption = 'S&mart tab'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object cbBackspaceUnindents: TCheckBox
          Left = 8
          Top = 56
          Width = 169
          Height = 17
          Caption = 'Backspace &unindents'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object cbGroupUndo: TCheckBox
          Left = 8
          Top = 76
          Width = 169
          Height = 17
          Caption = '&Group undo'
          Checked = True
          State = cbChecked
          TabOrder = 7
          OnClick = NotImplemented
        end
        object cbCursorBeyondEOF: TCheckBox
          Left = 8
          Top = 96
          Width = 169
          Height = 17
          Caption = 'Cursor beyond &EOF'
          TabOrder = 8
        end
      end
      object eTabStops: TEdit
        Left = 128
        Top = 224
        Width = 273
        Height = 21
        TabOrder = 2
        Text = '3 5'
      end
    end
    object tsColors: TTabSheet
      Caption = 'Colors'
      object lblColorSpeedSettingsFor: TLabel
        Left = 16
        Top = 11
        Width = 111
        Height = 13
        Alignment = taRightJustify
        Caption = 'Color SpeedSettings for'
      end
      object lblElement: TLabel
        Left = 8
        Top = 32
        Width = 41
        Height = 13
        Caption = '&Element:'
      end
      object lblColor: TLabel
        Left = 173
        Top = 32
        Width = 27
        Height = 13
        Caption = '&Color:'
      end
      object Label6: TLabel
        Left = 104
        Top = 224
        Width = 208
        Height = 13
        Caption = 'RAHLEditor1 will be created here in run-time'
        Visible = False
      end
      object cbColorSettings: TComboBox
        Left = 136
        Top = 6
        Width = 267
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbColorSettingsChange
        Items.Strings = (
          'Default'
          'Pascal'
          'CBuilder'
          'Sql'
          'Python'
          'Java'
          'VB'
          'Html'
          'Perl'
          'Ini'
          'Coco/R')
      end
      object lbElements: TListBox
        Left = 8
        Top = 48
        Width = 153
        Height = 121
        ExtendedSelect = False
        ItemHeight = 13
        Items.Strings = (
          'Whitespace'
          'Comment'
          'Reserved word'
          'Identifer'
          'Symbol'
          'String'
          'Number'
          'Preprocessor'
          'Declaration'
          'Function call'
          'Statement'
          'Plain text'
          'Marked block'
          'Right margin')
        Style = lbOwnerDrawFixed
        TabOrder = 1
        OnClick = lbElementsClick
        OnDrawItem = lbElementsDrawItem
      end
      object gbTextAttributes: TGroupBox
        Left = 300
        Top = 33
        Width = 104
        Height = 72
        Caption = 'Text attributes:'
        TabOrder = 2
        object cbBold: TCheckBox
          Left = 8
          Top = 17
          Width = 89
          Height = 17
          Caption = '&Bold'
          TabOrder = 0
          OnClick = ColorChanged
        end
        object cbItalic: TCheckBox
          Left = 8
          Top = 34
          Width = 89
          Height = 17
          Caption = '&Italic'
          TabOrder = 1
          OnClick = ColorChanged
        end
        object cbUnderline: TCheckBox
          Left = 8
          Top = 51
          Width = 89
          Height = 17
          Caption = '&Underline'
          TabOrder = 2
          OnClick = ColorChanged
        end
      end
      object gbUseDefaultsFor: TGroupBox
        Left = 299
        Top = 113
        Width = 104
        Height = 56
        Caption = 'Use defaults for:'
        TabOrder = 3
        object cbDefForeground: TCheckBox
          Left = 8
          Top = 17
          Width = 89
          Height = 17
          Caption = '&Foreground'
          TabOrder = 0
          OnClick = DefClick
        end
        object cbDefBackground: TCheckBox
          Left = 8
          Top = 34
          Width = 89
          Height = 17
          Caption = '&Background'
          TabOrder = 1
          OnClick = DefClick
        end
      end
      object Panel1: TPanel
        Left = 170
        Top = 48
        Width = 120
        Height = 120
        BevelOuter = bvNone
        TabOrder = 4
        object Cell0: TPanel
          Left = 1
          Top = 1
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clBlack
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnMouseDown = CellMouseDown
        end
        object Cell4: TPanel
          Tag = 4
          Left = 1
          Top = 31
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clNavy
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnMouseDown = CellMouseDown
        end
        object Cell8: TPanel
          Tag = 8
          Left = 1
          Top = 61
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clGray
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnMouseDown = CellMouseDown
        end
        object Cell12: TPanel
          Tag = 12
          Left = 1
          Top = 91
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clBlue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnMouseDown = CellMouseDown
        end
        object Cell1: TPanel
          Tag = 1
          Left = 31
          Top = 1
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clMaroon
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnMouseDown = CellMouseDown
        end
        object Cell5: TPanel
          Tag = 5
          Left = 31
          Top = 31
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clPurple
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnMouseDown = CellMouseDown
        end
        object Cell9: TPanel
          Tag = 9
          Left = 31
          Top = 61
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clRed
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          OnMouseDown = CellMouseDown
        end
        object Cell13: TPanel
          Tag = 13
          Left = 31
          Top = 91
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clFuchsia
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
          OnMouseDown = CellMouseDown
        end
        object Cell2: TPanel
          Tag = 2
          Left = 61
          Top = 1
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clGreen
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
          OnMouseDown = CellMouseDown
        end
        object Cell6: TPanel
          Tag = 6
          Left = 61
          Top = 31
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clTeal
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          OnMouseDown = CellMouseDown
        end
        object Cell10: TPanel
          Tag = 10
          Left = 61
          Top = 61
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clLime
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
          OnMouseDown = CellMouseDown
        end
        object Cell14: TPanel
          Tag = 14
          Left = 61
          Top = 91
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clAqua
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          OnMouseDown = CellMouseDown
        end
        object Cell3: TPanel
          Tag = 3
          Left = 91
          Top = 1
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clOlive
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
          OnMouseDown = CellMouseDown
        end
        object Cell7: TPanel
          Tag = 7
          Left = 91
          Top = 31
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clSilver
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 13
          OnMouseDown = CellMouseDown
        end
        object Cell11: TPanel
          Tag = 11
          Left = 91
          Top = 61
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clYellow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 14
          OnMouseDown = CellMouseDown
        end
        object Cell15: TPanel
          Tag = 15
          Left = 91
          Top = 91
          Width = 28
          Height = 28
          BevelInner = bvLowered
          BevelOuter = bvLowered
          Caption = 'FB'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 15
          OnMouseDown = CellMouseDown
        end
      end
    end
  end
  object bCancel: TButton
    Left = 352
    Top = 339
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object bOK: TButton
    Left = 268
    Top = 339
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object raColorSamples: TJvRegAuto
    RegPath = 'Software\nbs\RANotepad'
    Storage = raIniStrings
    IniFile = '$HOME/.JvInterpreterTest'
    IniStrings.Strings = (
      '[Default]'
      'Plain text'
      'Selected text'
      ''
      '[Pascal]'
      '{ Syntax highlighting }'
      'procedure TMain.RAHLEditor1ChangeStatus(Sender: TObject);'
      'const'
      '  Modi: array[boolean] of string[10] = ('#39#39', '#39'Modified'#39');'
      '  Modes: array[boolean] of string[10] = ('#39'Overwrite'#39', '#39'Insert'#39');'
      'begin'
      '  with StatusBar, RAHLEditor1 do'
      '  begin'
      '    Panels[0].Text := IntToStr(CaretY) + '#39':'#39' + IntToStr(CaretX);'
      '    Panels[1].Text := Modi[Modified];'
      '    if ReadOnly then'
      '      Panels[2].Text := '#39'ReadOnly'#39
      '    else if Recording then'
      '      Panels[2].Text := '#39'Recording'#39
      '    else'
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
      '    self.widgetName = '#39'__dialog__'#39
      '    Widget._setup(self, master, cnf)'
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
      
        '<p align="left">Download last JVCLmp;A Library version now - <fon' +
        't face="Arial"'
      
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
    AutoMode = False
    Left = 44
    Top = 320
  end
end
