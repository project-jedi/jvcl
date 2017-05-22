object frmMain: TfrmMain
  Left = 47
  Top = 91
  Width = 652
  Height = 531
  Caption = 'JvCaptionButton Demo'
  Color = clBtnFace
  Constraints.MinWidth = 644
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 589
    Height = 80
    Caption = 
      'This demo shows that the new RegisterWndProcHook method in JvWnd' +
      'ProcHook can'#13#10'handle several components subclassing the same con' +
      'trol witout losing track of the'#13#10'original WndProc regardless of ' +
      'the order of registration/unregistration'#13#10'It also shows that the' +
      ' WndProc link(s) are unaffected by RecreateWnd'#13#10'(which destroys ' +
      'the old window handle)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnAdd: TButton
    Left = 16
    Top = 120
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 0
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 16
    Top = 152
    Width = 75
    Height = 25
    Caption = '&Delete'
    TabOrder = 1
    OnClick = btnDeleteClick
  end
  object btnRecreateWnd: TButton
    Left = 16
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Recreate&Wnd'
    TabOrder = 2
    OnClick = btnRecreateWndClick
  end
  object lbButtons: TListBox
    Left = 110
    Top = 122
    Width = 505
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
end
