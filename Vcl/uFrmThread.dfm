object FrameThread: TFrameThread
  Left = 0
  Top = 0
  Width = 240
  Height = 250
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 240
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnlTop'
    TabOrder = 0
    object btnStart: TButton
      Left = 0
      Top = 0
      Width = 60
      Height = 24
      Align = alLeft
      Caption = 'Start'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnTerminate: TButton
      Left = 60
      Top = 0
      Width = 60
      Height = 24
      Align = alLeft
      Caption = 'Terminate'
      TabOrder = 1
      OnClick = btnTerminateClick
    end
    object btnDelete: TButton
      Left = 120
      Top = 0
      Width = 60
      Height = 24
      Align = alLeft
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 24
    Width = 240
    Height = 226
    Align = alClient
    Enabled = False
    TabOrder = 1
    object ediStatus: TEdit
      Left = 1
      Top = 1
      Width = 238
      Height = 21
      Align = alTop
      ReadOnly = True
      TabOrder = 0
    end
    object memLog: TMemo
      Left = 1
      Top = 22
      Width = 238
      Height = 203
      Align = alClient
      ReadOnly = True
      TabOrder = 1
    end
  end
end
