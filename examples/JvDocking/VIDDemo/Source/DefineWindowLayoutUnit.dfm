object DefineWindowLayoutForm: TDefineWindowLayoutForm
  Left = 408
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Define Window Layout'
  ClientHeight = 229
  ClientWidth = 364
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
  TextHeight = 14
  object Label1: TLabel
    Left = 9
    Top = 9
    Width = 66
    Height = 14
    Caption = 'View Name:'
  end
  object Label2: TLabel
    Left = 9
    Top = 65
    Width = 36
    Height = 14
    Caption = 'Views:'
  end
  object ViewName_Edit: TEdit
    Left = 9
    Top = 28
    Width = 244
    Height = 22
    TabOrder = 0
    OnChange = ViewName_EditChange
  end
  object Views_ListBox: TListBox
    Left = 9
    Top = 84
    Width = 244
    Height = 135
    ItemHeight = 14
    Sorted = True
    TabOrder = 1
    OnClick = Views_ListBoxClick
  end
  object Close_Button: TButton
    Left = 266
    Top = 9
    Width = 86
    Height = 25
    Caption = '&Close'
    TabOrder = 2
    OnClick = Close_ButtonClick
  end
  object Apply_Button: TButton
    Left = 266
    Top = 47
    Width = 86
    Height = 24
    Caption = 'A&pply'
    TabOrder = 3
    OnClick = Apply_ButtonClick
  end
  object Update_Button: TButton
    Left = 266
    Top = 84
    Width = 86
    Height = 25
    Caption = '&Update'
    TabOrder = 4
    OnClick = Update_ButtonClick
  end
  object Delete_Button: TButton
    Left = 266
    Top = 121
    Width = 86
    Height = 25
    Caption = '&Delete'
    TabOrder = 5
    OnClick = Delete_ButtonClick
  end
  object Rename_Button: TButton
    Left = 266
    Top = 159
    Width = 86
    Height = 24
    Caption = '&Rename'
    TabOrder = 6
    OnClick = Rename_ButtonClick
  end
  object Help_Button: TButton
    Left = 266
    Top = 196
    Width = 86
    Height = 25
    Caption = '&Help'
    TabOrder = 7
  end
end
