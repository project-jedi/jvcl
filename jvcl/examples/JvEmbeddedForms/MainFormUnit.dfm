object MainForm: TMainForm
  Left = 344
  Top = 153
  Width = 507
  Height = 454
  Caption = 'Embedded Forms Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 499
    Height = 427
    ActivePage = TabSheet1
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Example 1'
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 491
        Height = 16
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Shared Embedded Form'
        Color = clWindowText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object JvEmbeddedFormPanel1: TJvEmbeddedFormPanel
        Left = 0
        Top = 16
        Width = 491
        Height = 383
        AlwaysVisible = False
        FormLink = FirstEmbeddedForm.JvEmbeddedFormLink1
        Align = alClient
        Color = 12508350
        ParentColor = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
      end
      object btnDock: TButton
        Left = 360
        Top = 344
        Width = 107
        Height = 25
        Caption = 'Show Stand-Alone'
        TabOrder = 1
        OnClick = btnDockClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Example 2'
      ImageIndex = 1
      object Label4: TLabel
        Left = 0
        Top = 0
        Width = 491
        Height = 16
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Shared Embedded Form'
        Color = clWindowText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object JvEmbeddedFormPanel2: TJvEmbeddedFormPanel
        Left = 0
        Top = 16
        Width = 491
        Height = 383
        AlwaysVisible = False
        FormLink = FirstEmbeddedForm.JvEmbeddedFormLink1
        Align = alClient
        Color = 12508350
        ParentColor = False
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Example 3'
      ImageIndex = 2
      object Label3: TLabel
        Left = 0
        Top = 0
        Width = 491
        Height = 16
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'New Instance Embedded Form (sharing the deeply embedded form)'
        Color = clWindowText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object JvEmbeddedInstanceFormPanel1: TJvEmbeddedInstanceFormPanel
        Left = 0
        Top = 16
        Width = 491
        Height = 383
        AlwaysVisible = False
        FormLink = FirstEmbeddedForm.JvEmbeddedFormLink1
        Align = alClient
        Color = 12508350
        ParentColor = False
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'How it works'
      ImageIndex = 3
      object Memo1: TMemo
        Left = 8
        Top = 8
        Width = 481
        Height = 390
        Anchors = [akLeft, akTop, akRight, akBottom]
        Color = clWindowText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          
            'To use these components, you need to perform the following steps' +
            ':'
          '1. Your project must contain more than one form'
          '2. Forms must be auto-created to set up the link at design-time'
          
            '3. Drop a TJvEmbeddedFormLink component on each form that should' +
            ' be embedded in another form'
          
            '4. On each form that should embed another form, drop a TJvEmbedd' +
            'edFormPanel or a TJvEmbeddedInstanceFormPanel component'
          
            '5. Add the form to embed to the current forms uses clause (or se' +
            'lect File|Use Unit... from the Delphi menu)'
          
            '6. In each TJvEmbedded(Instance)FormPanel, set the FormLink prop' +
            'erty to point to the appropriate TJvEmbeddedFormLink component'
          '7. Run the program!')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
      end
    end
  end
end
