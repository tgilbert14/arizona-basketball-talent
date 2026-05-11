

select * from dbo.football;

Select COUNT(*) as TotalPlayers, State from dbo.football
group by State;

Select * from dbo.football
Where NationalRank = 'NA';

Select COUNT(*) from dbo.football
Where NationalRank = 'NA';

Select COUNT(*) from dbo.football
Where NationalRank != 'NA';

Select * from dbo.football
Where StateRank = 'NA';

-- count players by state and rank avergage
Select State, AVG(CAST(NationalRank AS FLOAT)) as AverageNationalRank, 
COUNT(*) as TotalPlayers from dbo.football
WHERE NationalRank != 'NA'
group by State;

