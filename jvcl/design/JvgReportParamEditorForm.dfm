object JvgReportParamEditor: TJvgReportParamEditor
  Left = 250
  Top = 212
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Report parameter'
  ClientHeight = 139
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 109
    Width = 413
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 245
      Top = 0
      Width = 168
      Height = 30
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BitBtn2: TBitBtn
        Left = 89
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 0
      end
      object BitBtn1: TBitBtn
        Left = 8
        Top = 2
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 1
      end
    end
  end
  object rgParameterType: TRadioGroup
    Left = 4
    Top = 4
    Width = 129
    Height = 101
    Caption = ' Parameter type '
    ItemIndex = 0
    Items.Strings = (
      'Text'
      'Radio group'
      'Checkbox'
      'Database source')
    TabOrder = 1
    OnClick = rgParameterTypeClick
  end
  object Notebook: TNotebook
    Left = 133
    Top = 1
    Width = 281
    Height = 109
    TabOrder = 2
    object TPage
      Left = 0
      Top = 0
      Caption = 'TextMask'
      object gbTextMask: TGroupBox
        Left = 7
        Top = 3
        Width = 268
        Height = 101
        Caption = ' Text mask '
        TabOrder = 0
        object Label1: TLabel
          Left = 9
          Top = 29
          Width = 29
          Height = 13
          Caption = 'Mask:'
        end
        object Label2: TLabel
          Left = 9
          Top = 65
          Width = 24
          Height = 13
          Caption = 'Test:'
        end
        object meTestMask: TMaskEdit
          Left = 45
          Top = 61
          Width = 215
          Height = 21
          TabOrder = 0
        end
        object eTextMask: TEdit
          Left = 45
          Top = 26
          Width = 214
          Height = 21
          TabOrder = 1
          OnChange = eTextMaskChange
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RadioGroup'
      object gbRadioItems: TGroupBox
        Left = 7
        Top = 3
        Width = 268
        Height = 101
        Caption = ' Radio Group '
        TabOrder = 0
        object Label5: TLabel
          Left = 8
          Top = 24
          Width = 28
          Height = 13
          Caption = 'Items:'
        end
        object lbRadioItems: TListBox
          Left = 39
          Top = 16
          Width = 179
          Height = 59
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbRadioItemsClick
        end
        object pbAddItem: TButton
          Left = 221
          Top = 46
          Width = 43
          Height = 23
          Caption = 'Add'
          TabOrder = 1
          OnClick = pbAddItemClick
        end
        object pbDeleteItem: TButton
          Left = 221
          Top = 17
          Width = 43
          Height = 23
          Caption = 'Delete'
          Enabled = False
          TabOrder = 2
          OnClick = pbDeleteItemClick
        end
        object eItemToAdd: TEdit
          Left = 39
          Top = 76
          Width = 179
          Height = 21
          TabOrder = 3
        end
        object pbInsertItem: TButton
          Tag = 1
          Left = 221
          Top = 74
          Width = 43
          Height = 23
          Caption = 'Insert'
          Enabled = False
          TabOrder = 4
          OnClick = pbAddItemClick
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Checkbox'
      object gbCheckbox: TGroupBox
        Left = 7
        Top = 3
        Width = 269
        Height = 102
        Caption = 'Checkbox'
        TabOrder = 0
        object Label7: TLabel
          Left = 9
          Top = 30
          Width = 39
          Height = 13
          Caption = 'Caption:'
        end
        object eCheckBox: TEdit
          Left = 8
          Top = 47
          Width = 252
          Height = 21
          TabOrder = 0
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'DataSource'
      object gbDataSource: TGroupBox
        Left = 7
        Top = 3
        Width = 268
        Height = 101
        Caption = ' Database source '
        TabOrder = 0
        object Label3: TLabel
          Left = 6
          Top = 28
          Width = 59
          Height = 13
          Caption = 'Table name:'
        end
        object Label4: TLabel
          Left = 8
          Top = 63
          Width = 54
          Height = 13
          Caption = 'Field name:'
        end
        object eTableName: TEdit
          Left = 68
          Top = 26
          Width = 192
          Height = 21
          TabOrder = 0
        end
        object eFieldName: TEdit
          Left = 68
          Top = 61
          Width = 192
          Height = 21
          TabOrder = 1
        end
      end
    end
  end
end
