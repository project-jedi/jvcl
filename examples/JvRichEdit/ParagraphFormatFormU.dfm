object ParagraphFormatForm: TParagraphFormatForm
  Left = 441
  Top = 248
  BorderStyle = bsDialog
  Caption = 'Paragraph'
  ClientHeight = 216
  ClientWidth = 393
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
  object OkBtn: TButton
    Left = 308
    Top = 10
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelBtn: TButton
    Left = 308
    Top = 41
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object IndentBox: TGroupBox
    Left = 12
    Top = 8
    Width = 185
    Height = 105
    Caption = ' Indentation '
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 20
      Width = 24
      Height = 13
      Caption = '&Left: '
      FocusControl = LeftIndent
    end
    object Label2: TLabel
      Left = 12
      Top = 48
      Width = 31
      Height = 13
      Caption = '&Right: '
      FocusControl = RightIndent
    end
    object Label3: TLabel
      Left = 12
      Top = 76
      Width = 44
      Height = 13
      Caption = '&First line: '
      FocusControl = FirstIndent
    end
    object LeftIndent: TJvSpinEdit
      Left = 68
      Top = 16
      Width = 97
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      Decimal = 0
      TabOrder = 0
    end
    object RightIndent: TJvSpinEdit
      Left = 68
      Top = 44
      Width = 97
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      Decimal = 0
      TabOrder = 1
    end
    object FirstIndent: TJvSpinEdit
      Left = 68
      Top = 72
      Width = 97
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      Decimal = 0
      TabOrder = 2
    end
  end
  object Alignment: TRadioGroup
    Left = 204
    Top = 8
    Width = 89
    Height = 105
    Caption = ' Alignment '
    ItemIndex = 0
    Items.Strings = (
      'Left'
      'Right'
      'Center')
    TabOrder = 1
  end
  object SpacingBox: TGroupBox
    Left = 12
    Top = 116
    Width = 281
    Height = 93
    Caption = ' Spacing '
    TabOrder = 2
    object Label4: TLabel
      Left = 56
      Top = 20
      Width = 70
      Height = 13
      Caption = 'Space &before: '
      FocusControl = SpaceBefore
    end
    object Label5: TLabel
      Left = 56
      Top = 44
      Width = 61
      Height = 13
      Caption = 'Space &after: '
      FocusControl = SpaceAfter
    end
    object Label6: TLabel
      Left = 56
      Top = 68
      Width = 66
      Height = 13
      Caption = 'Li&ne spacing: '
      FocusControl = LineSpacing
    end
    object SpaceBefore: TJvSpinEdit
      Left = 144
      Top = 16
      Width = 97
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      Decimal = 0
      MaxValue = 2147483647
      TabOrder = 0
    end
    object SpaceAfter: TJvSpinEdit
      Left = 144
      Top = 40
      Width = 97
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      Decimal = 0
      MaxValue = 2147483647
      TabOrder = 1
    end
    object LineSpacing: TJvSpinEdit
      Left = 144
      Top = 64
      Width = 97
      Height = 21
      Alignment = taRightJustify
      ButtonKind = bkStandard
      Decimal = 0
      Increment = 5
      MaxValue = 2147483647
      TabOrder = 2
    end
  end
end
