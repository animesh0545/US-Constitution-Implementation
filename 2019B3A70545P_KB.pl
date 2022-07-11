/*

NAME :- Animesh Bhargava
BITS Id :- 2019B3A70545P

NAME :- Sahil Gupta
BITS Id :- 2019B3A70154P

NAME :- Utkarsh Yashvardhan
BITS Id :- 2019B4A70704P

*/

% ----------- Facts copied for implementation of test cases---------------

age(rohan, 23).
citizen(rohan, 23).
age(meera, 30).
citizen(meera, 8).
age(david, 35).
citizen(david, 35).
age(leonard, 40).
citizen(leonard, 40).
age(amy, 38).
citizen(amy, 5).

stateOfUS(newHampshire).
stateOfUS(massachusetts).
stateOfUS(connecticut).
stateOfUS(newYork).
stateOfUS(newJersey).
stateOfUS(pennsylvania).
stateOfUS(delaware).
stateOfUS(maryland).
stateOfUS(virginia).
stateOfUS(northCarolina).
stateOfUS(southCarolina).
stateOfUS(georgia).

inhabitant(rohan, stateOfUS(massachusetts)).
inhabitant(meera, stateOfUS(newYork)).
inhabitant(david, stateOfUS(newHampshire)).
inhabitant(leonard, stateOfUS(maryland)).
inhabitant(amy, stateOfUS(southCarolina)).

monday(7, 1, 2019).
monday(2, 12, 2019).

% -----------------------------------------------------------------------

% ARTICLE I
% Section 1

consistsOf(congress, senate).
consistsOf(congress, houseOfRepresentatives).

% Section 2 

% composedOf(houseOfRepresentatives, membersOfHouseOfRepresentatives).
/* chosen/3 has first argument as the entity that is to be elected,
   second argument as the entity that is electing the entity in the first argument
   and third argument as the number of years for which the entity in the first argument is elected.
*/
chosen(membersOfHouseOfRepresentatives, electorsFromSeveralStates(membersOfHouseOfRepresentatives), 1). 
requisite(electorsFromSeveralStates(membersOfHouseOfRepresentatives), requisiteOfTheMostNumerousBranchOfTheStateLegislature).

qualified(X, houseOfRepresentatives) :- age(X, Y), Y >= 25,
                                        citizen(X, Z), Z >= 7.

qualifiedForGivenState(X, houseOfRepresentatives, stateOfUS(S)) :- age(X, Y), Y >= 25,
                                                                   citizen(X, Z), Z >= 7,
                                                                   not(inhabitant(X, stateOfUS(S))).

% Modifications from Amendment 14 Section 2 are made here.







made(enumerations(actual)) :- madeWithin(firstMeeting(congress), 3), madeWithin(everySubsequentTerm, 10), manner(asLawDirect).
numberOf(membersOfHouseOfRepresentatives, notExceed(forEvery30000(1))).
shallHave(eachState, Representatives) :- Representatives >= 1.

not(enumerations) :- 
entitled(choose(stateOfUS(newHampshire), 3)), 
entitled(choose(stateOfUS(massachusetts), 8)), 
entitled(choose(stateOfUS(rhodeIslandsAndProvidencePlantations), 1)), 
entitled(choose(stateOfUS(connecticut), 5)), 
entitled(choose(stateOfUS(newYork), 6)), 
entitled(choose(stateOfUS(newJersey), 4)), 
entitled(choose(stateOfUS(pennsylvania), 8)), 
entitled(choose(stateOfUS(delaware), 1)), 
entitled(choose(stateOfUS(maryland), 6)), 
entitled(choose(stateOfUS(virginia), 10)), 
entitled(choose(stateOfUS(northCarolina), 5)), 
entitled(choose(stateOfUS(southCarolina), 5)), 
entitled(choose(stateOfUS(georgia), 3)). 



issue(executiveAuthority, writsOfElection) :- vacancy(houseOfRepresentatives, stateOfUS(X)).

power(houseOfRepresentatives, impeachment(membersOfHouseOfRepresentatives)).

% Section 3
% Modified by the Amendment 17

composedOf(senate, senatorsFromEachState(2)).
chosen(senatorsFromEachState(2), peopleOfThatState, 6).
numberOfVotesEach(senatorsFromEachState(2), 1).
requisite(electorsFromSeveralStates(senatorsFromEachState(2)), requisiteOfTheMostNumerousBranchOfTheStateLegislature).
issue(executiveAuthority, writsOfElection) :- vacancy(senate, stateOfUS(X)).
empower(legislature, executiveAuthority, make(appointments(temporary), until(election(fills(vacancies))))) :- vacancy(senate, stateOfUS(X)).
not(construed(amendment(17, 1), affect(X))) :- member(X, [election, term(anySenator)]), before(valid(amendment(17, 1))).

dividedInto(senate, senateClass1, senateClass2, senateClass3).
vacate(senateClass1, 2). % seats of first class of senate is vacated after expiration of second year.
vacate(senateClass2, 4). % seats of second class of senate is vacated after expiration of fourth year.
vacate(senateClass3, 6). % seats of third class of senate is vacated after expiration of sixth year.

make(executiveAuthority, appointments(temporary), until(nextMeeting(legislature, forFillingVacancies))) :- vacancy(from(senate), happenBy(resignation), during(recessOf(legislatureOfThatState))).

qualifiedForGivenState(X, senate, stateOfUS(S)) :- age(X, Y), Y >= 30,
                                                   citizen(X, Z), Z >= 9,
                                                   not(inhabitant(X, stateOfUS(S))).

presidentOfSenate(vicePresident).
numberOfVotesEach(presidentOfSenate(vicePresident), 0) :- not(equallyDivided(senate)).
numberOfVotesEach(presidentOfSenate(vicePresident), 1) :- equallyDivided(senate).

power(senate, chooseOtherOfficesOfSenate).
power(senate, choosePresidentOfSenateProTemporeInAbsenceOfVP).

impeachment(senatorsFromEachState(2)) :- underOath(senate); underAffirmation(senate).
power(senate, impeachment(senatorsFromEachState(2))).
power(senate, impeachment(senatorsFromEachState(2))).

chiefJustice(president) :- tried(president).

convicted(senatorsFromEachState(2)) :- concurrence(senate, 2/3). % A member of senate can be convicted when there is a two-third concurrence in the senate.

furthestJudgement(senatorsFromEachState(2), X) :- impeachment(senatorsFromEachState(2)), member(X, [removalFromOffice, disqualificationToHoldOrEnjoyAnyOfficeOrHonor(trustOrProfitUnderTheUS)]).
liableAndSubjectToInAccordanceWithLaw(senatorsFromEachState(2), X) :- impeachment(senatorsFromEachState(2)), member(X, [indictment, Trial, Judgment, Punishment]).

% Section 4

power(legislatureOfThatState, choose(elections(X), Y)) :- member(X, [senate, houseOfRepresentatives]), member(Y, [time, place, manners]).
power(congress, makeOrAlter(elections(membersOfHouseOfRepresentatives), Y)) :- member(Y, [time, place, manners]).
power(congress, makeOrAlter(elections(senatorsFromEachState(2)), Y)) :- member(Y, [time, manners]).
meetingOfCongress(D,M,2019):- D=2, M=12.

% Modified through Amendment 20, Section 2.
meetingOfCongress(D, M, Y):- D=3, M=1.

% Section 5

judge(houseOfRepresentatives, X) :- member(X, [elections(membersOfHouseOfRepresentatives), returns(membersOfHouseOfRepresentatives), qualifications(membersOfHouseOfRepresentatives)]). 
judge(senate, X) :- member(X, [elections(senatorsFromEachState(2)), returns(senatorsFromEachState(2)), qualifications(senatorsFromEachState(2))]). 

constitute(majority(X), quorum) :- member(X, [houseOfRepresentatives, senate]), business(congress).
adjourn(business(congress)) :- smaller(number).

power(houseOfRepresentatives, compel(attendence(absent(membersOfHouseOfRepresentatives), using(X)))) :- member(X, [someManner, somePenalties]).
power(senate, compel(attendence(absent(senatorsFromEachState(2))), using(X))) :- member(X, [someManner, somePenalties]).

power(houseOfRepresentatives, Y) :- member(Y, [determine(rules(proceedings)), punish(membersOfHouseOfRepresentatives), expel(membersOfHouseOfRepresentatives)]).
power(senate, Y) :- member(Y, [determine(rules(proceedings)), punish(senatorsFromEachState(2)), expel(senatorsFromEachState(2))]).

punish(X) :- disorderlyBehaviour(X).
expel(membersOfHouseOfRepresentatives) :- concurrence(houseOfRepresentatives, 2/3).
expel(senatorsFromEachState(2)) :- concurrence(senate, 2/3).

keep(X, journal(proceedings)) :- member(X, [houseOfRepresentatives, senate]).
publish(X, keep(X, journal(proceedings))) :- member(X, [houseOfRepresentatives, senate]), not(require(secrecy(journal(proceedings)))).
enter(houseOfRepresentatives, journal(proceeding)) :- desire(membersOfHouseOfRepresentatives, 1/5).
enter(senate, journal(proceeding)) :- desire(senatorsFromEachState(2), 1/5).

% adjournD/2 takes first argument as a house of the Congress and second argument as the number of days it is going to be adjourned.
adjournD(houseOfRepresentatives, X) :- X > 3, consent(senate).
adjournD(senate, X) :- X > 3, consent(houseOfRepresentatives).

% adjournP/1 takes argument as a house of the Congress. The predicate is true when the house given as the argument to the predicate is adjourned to any other place than that in which the two houses shall be sitting.
adjournP(houseOfRepresentatives) :- consent(senate).
adjournP(senate) :- consent(houseOfRepresentatives).

% Section 6

receive(X, compensations(services, treasury(US))) :- member(X, [membersOfHouseOfRepresentatives, senatorsFromEachState(2)]).
privileged(membersOfHouseOfRepresentatives, not(arrest(duringOrGoingToOrReturningFrom(attendence(session(houseOfRepresentatives)))))) :- not(treason(membersOfHouseOfRepresentatives)), not(felony(membersOfHouseOfRepresentatives)), not(breach(membersOfHouseOfRepresentatives)).
privileged(senatorsFromEachState(2), not(arrest(during(attendence(session(senate)))))) :- not(treason(senatorsFromEachState(2))), not(felony(senatorsFromEachState(2))), not(breach(senatorsFromEachState(2))).
privileged(membersOfHouseOfRepresentatives, not(question(X, for(speech(houseOfRepresentatives), debate(houseOfRepresentatives)), in(anyOtherPlace)))).
privileged(senatorsFromEachState(2), not(question(X, for(speech(senate), debate(senate)), in(anyOtherPlace)))).
cannotBeAppointedTo(X, civilOffice(under(authorityOfUS), createdOrEmolumentsIncresed(during(timeForWhichThePersonIsElected)))) :- member(X, [membersOfHouseOfRepresentatives, senatorsFromEachState(2)]).
cannotBeElectedTo(person(holding(officeUnderUS)), X, during(continuationOfOffice)) :- member(X, [houseOfRepresentatives, senate]).

% Section 7

originate(allBills, houseOfRepresentatives).
proposeOrConcur(originate(allBills, houseOfRepresentatives), senate, with(amendments)).

presented(everyBill(passedTo(houseOfRepresentatives, senate), before(becomes(bill, law))), president).
sign(president, bill) :- approve(president, bill).
return(president, bill, to(houseOfOrigination), with(objections), enter(houseOfOrigination, journal(objections)), reconsider(journal(objections))) :- not(approve(president, bill)).
sendToReconsider(bill, journal(objections), otherHouse) :- after(reconsider(journal(objections)), agree(houseOfOrigination, 2/3, to(pass(bill)))).
becomeLaw(bill) :- approve(otherHouse, bill), approveByMajority(2/3).
inAllCases(votesDeterminedBy(yeas, nays), journalEntryForRespectiveHouses(personsVotingYeas), journalEntryForRespectiveHouses(personsVotingNays)).
happen(becomes(bill, law), manner(asIfPresidentSignedTheBill)) :- notReturns(president, bill, within(days(10, except(sunday)))), not(prevent(congress, returnOfBillBy(president))).
presentedTo(president, X) :- member(X, [order(toWhich(neccessary(concurrence(houseOfRepresentatives, senate)))), resolution(toWhich(neccessary(concurrence(houseOfRepresentatives, senate)))), vote(toWhich(neccessary(concurrence(houseOfRepresentatives, senate))))]), not(adjourned(congress)).
repass(X, repassBy(Y, byMajority(2/3), accordingTo(rulesAndLimitations(prescribedFor(bill))))) :- member(X, [order(toWhich(neccessary(concurrence(houseOfRepresentatives, senate)))), resolution(toWhich(neccessary(concurrence(houseOfRepresentatives, senate)))), member(Y, [houseOfRepresentatives, senate])]).


%Section 8

isPresent(X, [H|T]):- X=H; isPresent(X,T).
power(congress, X):- isPresent(X, [layTaxes, collectTaxes, borrowMoney, regulateCommerce(foreignNations, amongstates, indianTribes), establish(uniformRules(naturalisation, bankruptcies)), coinMoney, regulateMoney, fix(standardsofWeights), provide(punishment(counterfeiting)), establish(postOffice, postRoads), promote("progress of science and useful arts"), constituteTribunals, defineAndPunish(piracies, felonies), declareWar, raiseAndSupport(armies), provideAndMaintain(navy), makeGovernmentRules, makeLaws("which shall be necessary and proper for carrying into execution the foregoing powers, and all other powers vested by this Constitution in the government of the United States"), enforceByLegislation(amendmentXXIII), enforce(provisionsOfAmendementXIV), enforceByLegislation(amendmentXXIV)]).


%Section 9

migrationAllowed(X):- Y>=1808, migrationYear(X, Year).
suspend(habeasCorupus):- case(rebellion), case(invasion).
notAllowed(X):- isPresent(X, [pass("Bill of attainder of ex post facto law"), tax(articlesExportedFromAState), duty(articlesExportedFromAState), preference(regulationOfCommerce), drawMoneyWithoutAppropriationbyLaw(fromTreasury), titleofNobility]).


%Section 10

notAllowedA(X):- isPresent(X, [enter(state(treaty, alliance, confederation)), grant(letters(marque, reprisal), titleofNobility), coinMoney, emit(billsOfCredit), makeTenderBesides(gold, silver), pass(billofAttainder, exPostFactoLaw, "law impairing the obligation of contacts"), withoutConsentOfCongressLay(imposts, duties, dutyOfTonnage, inTimesOfPeace(keepTroops, keepShipsOfWar), enterAgreementOrCompact(anotherState, foreignPower), engageinWarUnless(invaded, imminentDanger))]).


%Article II

%Section 1
executivePower(president).
power(president, executivePower).
qualified(X,officeOfPresident):- citizen(X, Years), age(X,Age), Age>=35, Years>=14.
receiveCompensation(president).
beforeLeavingOffice(Oath):- Oath="I do solemnly
swear (or affirm) that I will faithfully execute the Office of President of the United States, and will to the best of my Ability, preserve, protect and defend the Constitution of the United States.".
numbersElected(X):- X= wholeNumber(numberOf(senators)).
elector(X):- notSenator(X), notRepresentative(X), notHoldingOfficeOfTrustOrProfit(X).

%Section 2
commanderInChiefOfArmy(president).
commanderInChiefOfNavy(president).
commanderInChiefOfMilitia(president).
mayRequiredOpinion(inWriting(president, principalOfficerOfExecutiveDepartments)).
power(president, grant(reprieve(offenseAgainstUS))).
power(president, grant(pardon(offenseAgainstUS))).
power(president, makeTreaties).
power(president, appoint(X)):- isPresent(X, [ambassadors, publicMinisters, consuls, judgesOfSupremeCourt, otherOfficersOfUnitedStates]).

%Section 3
shallGiveCongress(president, informationOfTheStateOfUnion).
shallReceive(president, X):- isPresent(X, [ambassadors, publicMinisters]).
shallEnsured(president, lawIsFaithfullyExecuted).
shall(commission(president, allOfficersOfUS)).

%Section 4
notImpeachment(false):- situation(X), isPresent(X, [treason, bribery, highCrimes, misdemeanours]).
shallBeRemovedFromOffice(Y):- notImpeachment(false), isPresent(Y, [president, vicePresident, civilOfficersOfUS]).

%ARTICLE 3
%Section 1
judicialPower(supremeCourt).
power(supremeCourt, judicialPower).
judicialPower(inferiorCourts).
power(inferiorCourts, judicialPower).
number(supremeCourt, 1).
number(inferiorCourts, decidedBy(congress)).
tenure(judge(supremeCourt), during(goodBehavior)).
tenure(judge(inferiorCourts), during(goodBehavior)).
receive(X, compensation) :- isPresent(X, [judge(supremeCourt), judge(inferiorCourts)]).
notDiminished(compensation, X) :- isPresent(X, [judge(supremeCourt), judge(inferiorCourts)]).

%Section 2

%extentOf/2 takes the power type in first argument and the extent of that power in second
extentOf(judicialPower, X):- isPresent(X, [casesUnder(X1), casesAffecting(X2), casesOf(X3), controversiesBetween(X4)]), isPresent(X1, [constitution, lawsOfTheUS, treaties]), isPresent(X2, [ambassadors, publicMinisters]), isPresent(X3, [admirality, maritimeJurisdiction]), isPresent(X4, ["United States and any other party", "Citizens of different States", "Citizens of same State claiming Lands under Grants of different States", "a State or Citizen and forerign States, Citizens or Subjects"]).

%supremeCourtJurisdiction/2 takes the type og jurisdiction in first argument and the cases concerning it in the second
supremeCourtJurisdiction(original, X) :- X = casesAffecting(Y), isPresent(Y, [ambassadors, publicMinisters]).

supremeCourtJurisdiction(appellate, Y) :- Y \= casesAffecting(Y), isPresent(Y, [ambassadors, publicMinisters]).

%trial/2 takes two arguments, first the body holding trials, the second the type of crime
trial(jury, X) :- X \= casesOfImpeachment.

%Section 3

%treason/1 returns if the input argument has committed treason or not, commits/2 tells if first argument has committed the act in the second argument
treason(X) :- commits(X, Y), isPresent(Y, ["levying War against the US", "adhering to the enemies of the US", "giving Aid and Comfort to the enemies of the US"]).
convicted(treason):- testimony(witness(2)).
convicted(treason):- confession(openCourt).

power(congress, declare(punishment(treason))).


%Article 4

%Section 1
fullFaithAndCredit(stateOfUS(S1), belongsTo(X, stateOfUS(S2))) :- isPresent(X, [publicActs, records, judicialProceedings]).

power(congress, prescribe(mannerOfProving(X))) :- stateOfUS(X), isPresent(X, [publicActs, records, judicialProceedings]).

%Section 2
entitled(citizen(stateOfUS(_)), Y) :- isPresent(Y, [citizenPriveleges, citizenImmunities]).

sent(X, stateOfUS(S2), stateOfUS(S1)) :- X = citizen(stateOfUS(S1)), chargedWith(X, Y) , isPresent(Y, [treason, felonies, otherCrimes]).

%Section 3
power(congress, admit(newStates, union)).
cannotForm(newState, X, without(consent(congress, legislatureOfThatState))) :- isPresent(X, [within(jurisdiction(stateOfUS(_))), junction(stateOfUS(_), Y)]), Y >= 2.

power(congress, X, Y) :- isPresent(X, [disposeOf, make(rulesAndRegulations)]), Y = property(belongsTo(unitedStates)).


%Section 4
guarantee(unitedStates, stateOfUS(_), formOfGovernment(republic)).
protectionAgainstInvasion(X) :- stateOfUS(X).

%Article V

amendment(valid):- ratifiedby(X,totalLegislature), X>totalLegislature*3/4, priorAmendement(doesNotViolate), noStateWithoutConsent( deprivedOfEqualSuffrageInSenate).
priorAmendement(doesNotViolate):- dateOfAmendment(Y), Y>1808.
priorAmendement(doesNotViolate):- dateOfAmendment(Y), Y<1808, notAffect(article1(ninthSection(firstClause, fourthClause))).

%Article 6

valid(X, before(adoptionOf(constitution))) :- isPresent(X, [debtsContracted, engagementsEntered]).

supremeLawOfTheLand(Y) :- isPresent(Y, [constitution, lawsOfTheUS, treaties]).

bound(judges, supremeLawOfTheLand(_)).

bound(X, Y) :- isPresent(X, [senators, membersOfHouseOfRepresentatives, memberOfStateLegislature, executiveOfficerOfAnyState, executiveOfficerOfTheUS, judicialOfficerOfAnyState, judicialOfficerOfTheUS]), isPresent(Y, oathOrAffirmation(toSupporr(constitution))).

notRequired(religiousTexxt, qualification(X)):- isPresent(X, [office, publicTrust]).

% Article 7

sufficient(ratification(conventions(nineStates)), establishment(constitution)).
done(convention(unanimousConsentOfTheStates(present(independenceDayOfUS(17.09.1787, 12))))) :- subscribed(Name), member(Name, [
name(washington, presidentAndDeputyFromVirginia),
name(johnLangdon, stateOfUS(newHampshire)), name(nicholasGilman, stateOfUS(newHampshire)),
name(nathanielGorham, stateOfUS(massachusetts)), name(rufusKing, stateOfUS(massachusetts)),
name(wmSamlJohnson, stateOfUS(connecticut)), name(rogerSherman, stateOfUS(connecticut)),
name(alexanderHamilton, stateOfUS(newYork)),
name(wilLivingston, stateOfUS(newJersey)), name(davidBrearley, stateOfUS(newJersey)), name(wmPaterson, stateOfUS(newJersey)), name(jonaDayton, stateOfUS(newJersey)),
name(bFranklin, stateOfUS(pennsylvania)), name(thomasMifflin, stateOfUS(pennsylvania)), name(robtMorris, stateOfUS(pennsylvania)), name(geoClymer, stateOfUS(pennsylvania)), name(thosFitzSimons, stateOfUS(pennsylvania)), name(jaredIngersoll, stateOfUS(pennsylvania)), name(jamesWilson, stateOfUS(pennsylvania)), name(gouvMorris, stateOfUS(pennsylvania)),
name(geoRead, stateOfUS(delaware)), name(gunningBedfordJun, stateOfUS(delaware)), name(johnDickinson, stateOfUS(delaware)), name(richardBassett, stateOfUS(delaware)), name(jacoBroom, stateOfUS(delaware)),
name(jamesMcHenry, stateOfUS(maryland)), name(danOfStThosJenifer, stateOfUS(maryland)), name(danlCarroll, stateOfUS(maryland)),
name(johnBlair, stateOfUS(virginia)), name(jamesMadisonJr, stateOfUS(virginia)),
name(jRutledge, stateOfUS(northCarolina)), name(richdDobbsSpaight, stateOfUS(northCarolina)), name(huWilliamson, stateOfUS(northCarolina)),
name(wmPaterson, stateOfUS(southCarolina)), name(charlesCotesworthPinckney, stateOfUS(southCarolina)), name(charlesPinckney, stateOfUS(southCarolina)), name(pierceButler, stateOfUS(southCarolina)),
name(williamFew, stateOfUS(georgia)), name(abrBaldwin, stateOfUS(georgia)),
name(williamJackson, attestedSecretary)
]).

present(convention(monday(17, 09, 1787), X)) :- member(X, [stateOfUS(newHampshire), stateOfUS(massachusetts), attendedBy(mrHamilton, from(stateOfUS(newYork))), stateOfUS(newJersey), stateOfUS(pennsylvania), stateOfUS(delaware), stateOfUS(maryland), stateOfUS(virginia), stateOfUS(northCarolina), stateOfUS(southCarolina), stateOfUS(georgia)]).
resolved(preceding(constitution), laidBefore(assembled(congress))).
opinion1(thisConvention, submitted(preceding(constitution), to(convention(delegates, chosen(in(eachState), by(peopleOfThatState), under(recommendation(of(legislatureOfThatState))), for(assentAndRatificationOfThatState)), giveOpinion(to(assembled(congress))))))).
resolved(opinion2(thisConvention, assembled(congress), shouldFix(day(appointment(electors(ratified(constitution))))))) :- convention(states(9), ratified(constitution)).
resolved(opinion2(thisConvention, assembled(congress), shouldFix(day(assemble(electors(to(vote(president), time(commencing(proceedings(under(constitution)))), place(commencing(proceedings(under(constitution))))))))))) :- convention(states(9), ratified(constitution)).
opinion3(thisConvention, after(publication, appoint(electors), appoint(membersOfHouseOfRepresentatives), appoint(senatorsFromEachState(2)))).
opinion4(thisConvention, meet(electors, day(election(president))), transmit(votes(electors), X, to(secretaryOfUS))) :- constitution(requires(X)), member(X, [certified, signed, sealed, directed]).
opinion5(thisConvention, convene(senatorsFromEachState(2), membersOfHouseOfRepresentatives, at(assigned(time, place)))).
opinion6(thisConvention, appoint(senators, presidentOfSenate, for(receiving(votes(president)), opening(votes(president)), counting(votes(president))))).
opinion6(thisConvention, after(appointment(president), execute(constitution, congress, president))).

go(washington, president).
go(wJackson, secretaryOfUS).

%Preamble to the Bill of Rights

reasonForAmmandments(prevent(X)) :- isPresent(X, [misconstruction, abuseOfPowes]).
conditionForAmmandments(VotesInFavorInCongress, TotalNumberOfCongress, StatesRatified, TotalStates) :- VotesInFavorInCongress >= 2/3 * TotalNumberOfCongress, StatesRatified >= 3/4 * TotalStates.

%Amendment I
not(power(congress, X)) :- isPresent(X, [makeLaws(establish(religion)), prohibit(freedomOf(Y)), petition(government, redressOf(grievances))]), isPresent(Y, [speech, press, peacefulAssembly]).

%Ammendment II
necessary(wellRegulatedMilitia, security(freeState)).
right(X, keepAndBear(arms)) :- citizen(X, Z).

%Ammendment III
not(quartered(soldier, house(X))) :- noConsent(X).

%Ammendment IV
rightOfCitizens(secure(X, Y)) :- isPresent(X, [persons, houses, papers, effecs]), isPresent(Y, [unreasonableSearches, seizures]).

%Ammendment V
not(heldToAnswer(X)) :- isPresent(X, [capitalCrime, infamousCrime]).
heldToAnswer(X) :- isPresent(X, [capitalCrime, infamousCrime]), indictmentOf(grandJury), not(case(Y), during(Z)), isPresent(Y, [landForces, navalForces, militia]), isPresent(Z, [war, publicDanger]).
not(subjectTo(sameOffence, 2)).
not(compelled(witness, against(themselves))).
not(deprivedOf(Y)) :- isPresent(Y, [life, liberty, property]).
not(taken(privateProperty, publicUse, notJustCompensation)).    

%Ammendment VI
rightsOfAccused(R) :- isPresent(R, [speedyTrial, publicTrial, trialByImpartialJury, informedOf(accusation(A)), confrontedWith(witness(against)), obtain(witness(favor)), assistanceOf(counsel)]), isPresent(A, [nature, cause]).

%Ammendment VII
rightOfTrial(valueInControversy(X)) :- X > 20.
not(reexamined(triedByJury(fact))).

%Ammendment VIII
not(required(bail(X))) :- X = excessive.
not(imposed(fine(X)) :- X = excessive).
not(inflicted(punishment(X))) :- x = cruel; unusual.

%Ammendment IX
not(rights(X)) :- deny(X, otherRights).

%Ammendment X
power(stateOfUS, X) :- not(power(unitedStates, X)), not(prohibited(power(stateOfUS, X))).
power(peopleOfTheUS, X) :- not(power(unitedStates, X)), not(prohibited(power(peopleOfTheUS, X))).

%AMMENDMENTS 11-27

%Ammendment XI
not(extentOf(judicialPower, X)) :- X = suitAgainst(US, Y), isPresent(Y, [citizen, foreignCitizen, foreignSubject]).

%Ammendment XII
vote(electors, ballot, president).
vote(electors, ballot, vicePresident).
not(vote(Elector, ballot, Y)) :- isPresent(Y, [PresidentCandidate, VicePresidentCandidate]), stateOf(Elector) =:= stateOf(PresidentCandidate), stateOf(Elector) =:= stateOf(VicePresidentCandidate).
counting(votes(X), presidentOfSenate, presenceOf(Y)) :- isPresent(X, [president, vicePresident]), isPresent(Y, [senate, houseOfRepresentatives]).
isPresident(X) :- maxVotes(X), numberOfVotes(X) >= 1/2 * TotalVotes.
electPresident(houseOfRepresentatives) :- not(isPresident(_)).
isVicePresident(X) :- maxVotes(X), numberOfVotes(X) >= 1/2 * TotalVotes.
electVicePresident(houseOfRepresentatives) :- not(isVicePresident(_)).
not(eligible(vicePresident)) :- not(eligible(president)).

%Ammendment XIII
notExist(X) :- isPresent(X, [slavery, involuntaryServitude]), X \= punishmentForCrime.
power(congress, enforce(notExist(X))) :- isPresent(X, [slavery, involuntaryServitude]).

%Amendment XIV

%Section I
stateShallNotDepriveCitizen(withoutDueProcessOfLaw(X)):- isPresent(X, [life, liberty, property, equalProtectionOfTheLaws]).
stateShallNotDepriveCitizen(citizenPriveleges, citizenImmunities).



%Section II -affects Article I Section II --changed by Amendment 26 Section 1
representativeAppointment(X, stateofUS(X)):- X=ratioOf(totalWholeNumberPersons(stateOf(US)- notTaxed(Indians)-  maleDeniedRightToVote(age(_, Age)))), Age>=21.

%Section III
shallNotbe(Person, Y):- isPresent(Y, [senator, representativeInCongress, electorOfPresident, electorOfVicePresident, holderOfOffice(underAnyState(civil, military))]), hasTakenOathAs(Z), isPresent(Z, [memberOfCongress, officerOfUS, memberOfStateLegislature, executiveOfAnyState, judicialOfficerOfAnyState]).
shallbe(Person, Y):-  isPresent(Y, [senator, representativeInCongress, electorOfPresident, electorOfVicePresident, holderOfOffice(underAnyState(civil, military))]), hasTakenOathAs(Z), isPresent(Z, [memberOfCongress, officerOfUS, memberOfStateLegislature, executiveOfAnyState, judicialOfficerOfAnyState]), voteOfCongress(Number, totalNumberOfCongress), Number>=totalNumberOfCongress*2/3.

%Section IV
usOrAnyStateShallNotLegallyAssumeOrPay(debtOrObligationOf(X)):- isPresent(X, [insurrectionAgainstUS, rebellionAgainstUS, lossOrEmancipationOfSlave]).
usShallPay(X):- isPresent(X, [debtsIncurredForPensions, bountiesForServices(suppressing(insurrection, rebellion))]).


%Section V
power(congress, enforce(provisionsOfAmendementXIV)).
%added in power predicate initially.

%Amendment XV

% Section 1
right(X, vote) :- citizen(X, Z), age(X, A), A >= 18.
not(deniedOrAbridged(right(X, vote), onAccountOf(Y))) :- member(Y, [race, color, previousConditionOfServitude]).
amendment(15, 1) :- not(deniedOrAbridged(right(X, vote), onAccountOf(Y))).

% Section 2

power(congress, enforce(amendment(15, 1), legislation(appropriate))).

%Amendment XVI

power(congress, layAndCollect(taxes(incomes(anySource), without(apportionmentAmong(states), regard(censusOrEnumeration))))).

%Amendment XVII
% Modifications are done in Article 1, Section 3 itself.

%Amendment XVIII

% Section 1

prohibited(X, intoxicatingLiquors) :- member(X, [manufacture, sale, importationWithin(USAndAllTerritorySubjectToTheJurisdictionForBeveragePurposes), expotationFrom(USAndAllTerritorySubjectToTheJurisdictionForBeveragePurposes)]), after(ratification(amendment(18, 1)), 1). 

% Section 2

power(congress, enforce(amendment(18, 1), legislation(appropriate))).
power(severalStates, enforce(amendment(19, 1), legislation(appropriate))).

% Section 3

not(inoperative(amendment(19, 1))) :- ratified(amendment(19, 1), to(constitution), by(legislaturesOfSeveralStates), asProvidedIn(constitution), within(years(7)), from(dateOfSubmission(to(states), by(congress)))). 

%Amendment XIX

not(deniedOrAbridged(right(vote(X)), onAccountOf(sex))).
amendment(19, 1) :- not(deniedOrAbridged(right(vote(X)), onAccountOf(sex))).
power(congress, enforce(amendment(19, 1), legislation(appropriate))).

%Amendment XX

% Section 1

term(X, end(time(noon, 20/01))) :- member(X, [president, vicePresident]).
term(X, end(time(noon, 03/01))) :- member(X, [membersOfHouseOfRepresentatives, senatorsFromEachState(2)]).

% Section 2
% Modifications are done in Article 1, Section 4 itself.

% Section 3

become(vicePresident, president, permanently) :- died(president, beginning(term(president))).
become(vicePresident, president, until(qualified(president))) :- not(chosen(president, before(beginning(term(president))))), not(qualify(president)).
declare(congress, X) :- not(elected(president)), not(qualified(vicePresident)), member(X, [acting(president), mannerOfSelection(acting(president))]).
vacate(acting(president)) :- qualified(president); qualified(vicePresident).

% Section 4

provideByLaw(congress, inCase(death(candidate(for(president, toBeChosenBy(membersOfHouseOfRepresentatives))))), whenever(right(choice), developed(congress))).
provideByLaw(congress, inCase(death(candidate(for(vicePresident, toBeChosenBy(senatorsFromEachState(2))))), whenever(right(choice), developed(congress)))). 

% Section 5

takeEffect(amendment(20, 1), time(noon, 15/10)) :- ratification(amendment(20, 5)).
takeEffect(amendment(20, 2), time(noon, 15/10)) :- ratification(amendment(20, 5)).

% Section 6

not(inoperative(amendment(20, 1)) :- ratified(amendment(20, 6), to(constitution), by(legislaturesOfThreeFourthsSeveralStates), asProvidedIn(constitution), within(years(7)), from(dateOfSubmission(to(states), by(congress))))). 

%Amendment XXI

%Section I
repealed(amendmentXXI).

%Section II
prohibited(inViolationOfLaws(ofIntoxicatingLiquors(transportation, importation, possession, delivery))).

%Section III
valid(amendmentXXI):- ratifiedWithinSeveralYearsOfSubmissionToStatesByCongress(by(conventionInTheSeveralStates)).


%Amendment XXII

%Section I
notElectedAsPresident(Person, president):- president(Person, Times), Times>2, applies(amendmentXXII).
notElectedAsPresident(Person, president):- president(Person, Times), Times>1, servedInTheTermToWhichAnotherPresidentWasElected(Person), applies(amendmentXXII).
applies(amendmentXXII):- notPresidentWhenArticleProposed(Person).
amendmentShallNot(prevent(president, personActingAsPresident)):- amendmentBecomesOperativeDuringTenureOf(Person).

%Section II
valid(amendmentXXII):- ratifiedWithinSeveralYearsOfSubmissionToStatesByCongress(by(legislaturesOf(totalLegislatures*3/4))).


%Amendment XXIII

%Section I
numberOfElectorsOfPresident(X):- X=wholeNumberOf(senators, representativesInCongress).
shallMeetInDistrict(electors).
performDuties(electors, asProvidedBy(amendmentXII)).

%Section II
%power(congress, enforceByLegislation(Amendment XXIII)).
%added to power predicate initially.

%Amendment XXIV

%Section I
shallNotBeDeniedByUS(rightOfCitizens(toVote(elections(president, vicePresident, electorsOfPresident, electorsOfVicePresident)))):- reason(failureToPay(pollTax, otherTax)).

%Section II
%power(congress, enforceByLegislation(Amendment XXIV)).
%added to power predicate initially.

%Amendment XXV

%Section I

%Section I
president(vicePresident):- removalOf(president).
president(vicePresident):- deathOf(president).
president(vicePresident):- resignationOf(president).

%Section II
power(president, nominate(vicePresident)):- not(officeOfVicePresident), confirmationBy(majorityVote(bothHousesOfCongress)).

%Section III
power(vicePresident, power(president, X)):- transmittedByPresident(to(presidentProTemporeOfSenate(unableToDischargePowersAndDutiesOf(office)))).

%Section IV
power(vicePresident, power(president, X)):- transmitToPresidentProTemporeAndSpeakerOfHouseOfRepresentatives([vicePresident, majorityOfPrincipalOfficersOfExecutiveDepartments], writtendeclaration(presidentUnableToDisrchargePowerAndDuties)), validPowerTransfer();

validPowerTransfer(true):- withinFourDaystransmitToPresidentProTemporeAndSpeakerOfHouseOfRepresentatives([vicePresident, majorityOfPrincipalOfficersOfExecutiveDepartments], writtendeclaration(presidentUnableToDisrchargePowerAndDuties)), congress(Y), isPresent(Y, [congressOn(decides(true)), congressOf(decidesWithinTwentyOneDays(true))]), voteOf(bothHouses*2/3). 

%Amendment XVI

% Section 1


not(deniedOrAbridged(right(vote(X)), onAccountOf(age(X, Y)))) :- Y >= 18.
amendment(26, 1) :- not(deniedOrAbridged(right(vote(X)), onAccountOf(age(X, Y)))).

% Section 2

power(congress, enforce(amendment(26, 1), legislation(appropriate))).

%Amendment XVII

variation(X, compensations(services, tresury(US))) :- member(X, [membersOfHouseOfRepresentatives, senatorsFromEachState(2)]), intervention(election(membersOfHouseOfRepresentatives)).


%END