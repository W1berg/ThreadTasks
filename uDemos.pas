// Copyright (C) 2023 Pontus Wiberg <wiberg.public@outlook.com>
// Licensed under GPLv2 or later version. Contact for any licensing concerns

unit uDemos;

interface

procedure Start;
procedure Stop;
procedure Refresh;

implementation

uses
  uTypes,
  uTask,

  Classes,
  Generics.Collections,
  SysUtils;

var
  FFac: IFac;
  FCluster: ITaskCluster;

procedure Demo1LogOnce;
var
  LProc: TProcArg1<ITask<Integer>>;
  LTaskOnce1, LTaskOnce2: ITask<Integer>;
begin
  LProc := procedure(const ATask: ITask<Integer>)
    begin
      ATask.Log('A log');
    end;

  LTaskOnce1 := TTask<Integer>.Create(tcFunc, 'Func log once', LProc);
  LTaskOnce2 := TTask<Integer>.Create(tcThread, 'Thread log once', LProc);

  FCluster.Add(LTaskOnce1);
  FCluster.Add(LTaskOnce2);
end;

procedure Demo2Counter;
var
  LProc: TProcArg1<ITask<Integer>>;
  LTaskCount1, LTaskCount2: ITask<Integer>;
begin
  LProc := procedure(const ATask: ITask<Integer>)
    begin
      while ATask.Ticking do
      begin
        ATask.SetData(ATask.Data + 1);
        ATask.Log(ATask.Data.ToString);
        ATask.ThreadSleep(500);
      end;
    end;

  LTaskCount1 := TTask<Integer>.Create(tcFunc, 'Func counter', LProc);
  LTaskCount2 := TTask<Integer>.Create(tcThread, 'Thread counter', LProc);

  FCluster.Add(LTaskCount1);
  FCluster.Add(LTaskCount2);
end;

procedure Demo3SharedLockAdd;
var
  LProc: TProcArg1<ITask<ILock<Integer>>>;
  LTask1, LTask2: ITask<ILock<Integer>>;
  LData: ILock<Integer>;
const
  cCountTo = 100;
begin
  LProc := procedure(const ATask: ITask < ILock < Integer >> )
    begin
      for var I := 1 to cCountTo do
      begin
        while not ATask.Data.WriterGet do
          if ATask.IsTerminated then
            Exit;

        try
          ATask.Data.SetData(ATask.Data.Data + 1);
          ATask.Log(ATask.GetName + ':' + ATask.Data.Data.ToString);
        finally
          ATask.Data.WriterRel;
        end;

        if ATask.IsTerminated then
          Exit;

        ATask.ThreadSleep(50);
      end;
    end;

  LData := FGet.Lock<Integer>;

  LTask1 := TTask < ILock < Integer >>.Create(tcThread, 'Thread1 shared lock add ' + cCountTo.ToString, LProc, LData);
  LTask2 := TTask < ILock < Integer >>.Create(tcThread, 'Thread2 shared lock add ' + cCountTo.ToString, LProc, LData);

  FCluster.Add(LTask1);
  FCluster.Add(LTask2);
end;

procedure Start;
begin
  FFac := FGet.Fac;
  FCluster := FFac.TaskClusterMain;

  Demo1LogOnce;
  Demo2Counter;
  Demo3SharedLockAdd;

  FCluster.Run;
end;

procedure Stop;
begin
  FCluster.Terminate;
  Refresh;
end;

procedure Refresh;
begin
  if Assigned(FCluster) then
    FCluster.Refresh;
end;

end.
