object FormFin: TFormFin
  Left = 254
  Top = 290
  BorderStyle = bsDialog
  Caption = 'BU - FindFirst/FindNext/FindClose'
  ClientHeight = 368
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Image2: TImage
    Left = 8
    Top = 10
    Width = 100
    Height = 350
    AutoSize = True
  end
  object PageControl1: TPageControl
    Left = 116
    Top = 10
    Width = 303
    Height = 303
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ParentShowHint = False
      ShowHint = True
      TabVisible = False
      object StaticText7: TStaticText
        Left = 4
        Top = 8
        Width = 275
        Height = 39
        AutoSize = False
        Caption = 
          'This expert will help you building a findfirst, findnext, '#13#10'find' +
          'close module. It can be either in a separated '#13#10'procedure or dir' +
          'ectly in your code.'
        TabOrder = 0
      end
      object RadioGroup1: TRadioGroup
        Left = 4
        Top = 54
        Width = 285
        Height = 81
        ItemIndex = 0
        Items.Strings = (
          'Generate in a procedure'
          'Generate in current code')
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      TabVisible = False
      object GroupBox4: TGroupBox
        Left = 4
        Top = 44
        Width = 285
        Height = 119
        Caption = '[ Attributes ]'
        TabOrder = 0
        object CheckBox8: TCheckBox
          Left = 12
          Top = 94
          Width = 97
          Height = 17
          Caption = 'Any file'
          TabOrder = 3
        end
        object CheckBox5: TCheckBox
          Left = 138
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Volume ID files'
          TabOrder = 4
        end
        object CheckBox6: TCheckBox
          Left = 138
          Top = 43
          Width = 97
          Height = 17
          Caption = 'Directory files'
          TabOrder = 5
        end
        object CheckBox7: TCheckBox
          Left = 138
          Top = 68
          Width = 97
          Height = 17
          Caption = 'Archive files'
          TabOrder = 6
        end
        object CheckBox4: TCheckBox
          Left = 12
          Top = 68
          Width = 97
          Height = 17
          Caption = 'System files'
          TabOrder = 2
        end
        object CheckBox3: TCheckBox
          Left = 12
          Top = 43
          Width = 97
          Height = 17
          Caption = 'Hidden files'
          TabOrder = 1
        end
        object CheckBox2: TCheckBox
          Left = 12
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Read Only files'
          TabOrder = 0
        end
      end
      object StaticText1: TStaticText
        Left = 4
        Top = 8
        Width = 275
        Height = 31
        AutoSize = False
        Caption = 
          'Now, you have to choose the attributes for '#13#10'the files you want ' +
          'to search and the mask.'
        TabOrder = 2
      end
      object GroupBox5: TGroupBox
        Left = 4
        Top = 166
        Width = 285
        Height = 49
        Caption = '[ File mask ]'
        TabOrder = 1
        object Edit5: TEdit
          Left = 8
          Top = 18
          Width = 265
          Height = 21
          TabOrder = 0
          Text = '*.*'
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
      TabVisible = False
      object GroupBox2: TGroupBox
        Left = 4
        Top = 6
        Width = 285
        Height = 87
        Caption = '[ Procedure Options ]'
        TabOrder = 0
        object Label1: TLabel
          Left = 10
          Top = 26
          Width = 78
          Height = 13
          Caption = 'Procedure name'
        end
        object Edit1: TEdit
          Left = 94
          Top = 22
          Width = 181
          Height = 21
          TabOrder = 0
        end
        object CheckBox1: TCheckBox
          Left = 12
          Top = 52
          Width = 201
          Height = 17
          Caption = 'Search in directory recursively'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TabSheet4'
      ImageIndex = 3
      TabVisible = False
      object GroupBox3: TGroupBox
        Left = 4
        Top = 6
        Width = 287
        Height = 87
        Caption = '[ Code Options (name of variables) ]'
        TabOrder = 0
        object Label2: TLabel
          Left = 8
          Top = 22
          Width = 114
          Height = 13
          Caption = 'Variable containing path'
        end
        object Label3: TLabel
          Left = 8
          Top = 44
          Width = 101
          Height = 13
          Caption = 'TSearchRec variable'
        end
        object Label4: TLabel
          Left = 8
          Top = 66
          Width = 85
          Height = 13
          Caption = 'Temporaty integer'
        end
        object Edit2: TEdit
          Left = 128
          Top = 18
          Width = 151
          Height = 21
          TabOrder = 0
        end
        object Edit3: TEdit
          Left = 128
          Top = 40
          Width = 151
          Height = 21
          TabOrder = 1
        end
        object Edit4: TEdit
          Left = 128
          Top = 62
          Width = 151
          Height = 21
          TabOrder = 2
        end
      end
    end
  end
  object BUButton4: TJvButton
    Left = 330
    Top = 330
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = BUButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton5: TJvButton
    Left = 228
    Top = 330
    Width = 75
    Height = 25
    Caption = '&Next'
    Default = True
    TabOrder = 0
    OnClick = BUButton5Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton6: TJvButton
    Left = 126
    Top = 330
    Width = 75
    Height = 25
    Caption = '&Previous'
    Enabled = False
    TabOrder = 1
    OnClick = BUButton6Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
end
