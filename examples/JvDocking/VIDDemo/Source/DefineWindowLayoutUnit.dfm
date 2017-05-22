object DefineWindowLayoutForm: TDefineWindowLayoutForm
  Left = 408
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Define Window Layout'
  ClientHeight = 196
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = CHINESEBIG5_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #203#206#204#229
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 57
    Height = 12
    Caption = 'View Name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 31
    Height = 12
    Caption = 'Views:'
  end
  object ViewName_Edit: TEdit
    Left = 8
    Top = 24
    Width = 209
    Height = 22
    TabOrder = 0
    OnChange = ViewName_EditChange
  end
  object Views_ListBox: TListBox
    Left = 8
    Top = 72
    Width = 209
    Height = 116
    ItemHeight = 14
    Sorted = True
    TabOrder = 1
    OnClick = Views_ListBoxClick
  end
  object Close_Button: TButton
    Left = 228
    Top = 8
    Width = 74
    Height = 21
    Caption = '&Close'
    TabOrder = 2
    OnClick = Close_ButtonClick
  end
  object Apply_Button: TButton
    Left = 228
    Top = 40
    Width = 74
    Height = 21
    Caption = 'A&pply'
    TabOrder = 3
    OnClick = Apply_ButtonClick
  end
  object Update_Button: TButton
    Left = 228
    Top = 72
    Width = 74
    Height = 21
    Caption = '&Update'
    TabOrder = 4
    OnClick = Update_ButtonClick
  end
  object Delete_Button: TButton
    Left = 228
    Top = 104
    Width = 74
    Height = 21
    Caption = '&Delete'
    TabOrder = 5
    OnClick = Delete_ButtonClick
  end
  object Rename_Button: TButton
    Left = 228
    Top = 136
    Width = 74
    Height = 21
    Caption = '&Rename'
    TabOrder = 6
    OnClick = Rename_ButtonClick
  end
  object Help_Button: TButton
    Left = 228
    Top = 168
    Width = 74
    Height = 21
    Caption = '&Help'
    TabOrder = 7
  end
end
