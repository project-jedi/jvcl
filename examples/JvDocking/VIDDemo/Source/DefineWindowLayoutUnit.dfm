object DefineWindowLayoutForm: TDefineWindowLayoutForm
  Left = 408
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Define Window Layout'
  ClientHeight = 261
  ClientWidth = 416
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
  TextHeight = 16
  object Label1: TLabel
    Left = 11
    Top = 11
    Width = 67
    Height = 16
    Caption = 'View Name:'
  end
  object Label2: TLabel
    Left = 11
    Top = 75
    Width = 36
    Height = 16
    Caption = 'Views:'
  end
  object ViewName_Edit: TEdit
    Left = 11
    Top = 32
    Width = 278
    Height = 24
    TabOrder = 0
    OnChange = ViewName_EditChange
  end
  object Views_ListBox: TListBox
    Left = 11
    Top = 96
    Width = 278
    Height = 155
    ItemHeight = 16
    Sorted = True
    TabOrder = 1
    OnClick = Views_ListBoxClick
  end
  object Close_Button: TButton
    Left = 304
    Top = 11
    Width = 99
    Height = 28
    Caption = '&Close'
    TabOrder = 2
    OnClick = Close_ButtonClick
  end
  object Apply_Button: TButton
    Left = 304
    Top = 53
    Width = 99
    Height = 28
    Caption = 'A&pply'
    TabOrder = 3
    OnClick = Apply_ButtonClick
  end
  object Update_Button: TButton
    Left = 304
    Top = 96
    Width = 99
    Height = 28
    Caption = '&Update'
    TabOrder = 4
    OnClick = Update_ButtonClick
  end
  object Delete_Button: TButton
    Left = 304
    Top = 139
    Width = 99
    Height = 28
    Caption = '&Delete'
    TabOrder = 5
    OnClick = Delete_ButtonClick
  end
  object Rename_Button: TButton
    Left = 304
    Top = 181
    Width = 99
    Height = 28
    Caption = '&Rename'
    TabOrder = 6
    OnClick = Rename_ButtonClick
  end
  object Help_Button: TButton
    Left = 304
    Top = 224
    Width = 99
    Height = 28
    Caption = '&Help'
    TabOrder = 7
  end
end
