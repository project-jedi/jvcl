object FindReplaceMainForm: TFindReplaceMainForm
  Left = 374
  Top = 141
  Width = 473
  Height = 363
  Caption = 'FindReplace test/demo program'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 450
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 81
    Width = 465
    Height = 233
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      
        '{***************************************************************' +
        '***'
      ''
      '                       JEDI-VCL Demo'
      ''
      ' Copyright (C) 2002 Project JEDI'
      ''
      ' Original author:'
      ''
      ' Contributor(s):'
      ''
      
        ' You may retrieve the latest version of this file at the JEDI-JV' +
        'CL'
      ' home page, located at http://jvcl.sourceforge.net'
      ''
      ' The contents of this file are used with permission, subject to'
      ' the Mozilla Public License Version 1.1 (the "License"); you may'
      
        ' not use this file except in compliance with the License. You ma' +
        'y'
      ' obtain a copy of the License at'
      ' http://www.mozilla.org/MPL/MPL-1_1Final.html'
      ''
      ' Software distributed under the License is distributed on an'
      ' "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or'
      ' implied. See the License for the specific language governing'
      ' rights and limitations under the License.'
      ''
      
        '****************************************************************' +
        '**}')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 8
      Width = 388
      Height = 64
      Caption = 
        'Test program for the JvFindReplace unit.'#13#10'Has two different Find' +
        ' systems: one using the built in dialogs;'#13#10'one using InputQuery ' +
        'to handle user selection.'#13#10'See JvFindReplace.pas for more detail' +
        's.'
    end
  end
  object FindReplace1: TJvFindReplace
    Fast = True
    Options = [frDown, frShowHelp]
    EditControl = Memo1
    KeepText = True
    OnFind = FindReplace1Find
    OnReplace = FindReplace1Replace
    OnNotFound = FindReplace1NotFound
    Left = 48
    Top = 104
  end
  object MainMenu1: TMainMenu
    Left = 128
    Top = 104
    object Search1: TMenuItem
      Caption = 'Find (&Dialogs)'
      object Find1: TMenuItem
        Caption = 'Find...'
        ShortCut = 16450
        OnClick = Find1Click
      end
      object Replace1: TMenuItem
        Caption = 'Replace...'
        ShortCut = 16466
        OnClick = Replace1Click
      end
      object FindAgain1: TMenuItem
        Caption = 'Find Again'
        ShortCut = 114
        OnClick = FindAgain1Click
      end
    end
    object Serachown1: TMenuItem
      Caption = 'Find (&Own dialogs)'
      object Find2: TMenuItem
        Caption = 'Find...'
        ShortCut = 16454
        OnClick = Find2Click
      end
      object Replace2: TMenuItem
        Caption = 'Replace...'
        ShortCut = 16464
        OnClick = Replace2Click
      end
      object FindAgain2: TMenuItem
        Caption = 'Find Again'
        ShortCut = 115
        OnClick = FindAgain2Click
      end
    end
    object Options1: TMenuItem
      Caption = 'O&ptions'
      object Rememberlastsearch1: TMenuItem
        Caption = 'Remember last search'
        Checked = True
        OnClick = Rememberlastsearch1Click
      end
    end
  end
end
