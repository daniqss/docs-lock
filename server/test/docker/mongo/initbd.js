db = db.getSiblingDB("docs_lock");
db.createCollection("users");
db.createCollection("skills");
db.createCollection("sections");
db.createCollection("notes");

const users = db.getCollection("users");
const skills = db.getCollection("skills");
const sections = db.getCollection("sections");
const notes = db.getCollection("notes");

const sampleUsers = [
  { gitUsername: "user1", realName: "User One" },
  { gitUsername: "user2", realName: "User Two" },
  { gitUsername: "user3", realName: "User Three" },
];

const sampleSkills = [
  { skillName: "JavaScript", participants: [] },
  { skillName: "Python", participants: [] },
  { skillName: "MongoDB", participants: [] },
];

const sampleSections = [
  {
    sectionName: "Frontend Development",
    skill: { id: 1, name: "JavaScript" },
    participants: ["user1", "user2"],
  },
  {
    sectionName: "Backend Development",
    skill: { id: 2, name: "Python" },
    participants: ["user2", "user3"],
  },
  {
    sectionName: "Database Management",
    skill: { id: 3, name: "MongoDB" },
    participants: ["user1", "user3"],
  },
];

const sampleNotes = [
  { userId: "user1", sectionId: "section1", content: "Learning about React" },
  {
    userId: "user2",
    sectionId: "section1",
    content: "Working on component design",
  },
  {
    userId: "user2",
    sectionId: "section2",
    content: "Setting up a Flask server",
  },
  { userId: "user3", sectionId: "section2", content: "Building API endpoints" },
  {
    userId: "user1",
    sectionId: "section3",
    content: "Understanding NoSQL databases",
  },
  {
    userId: "user3",
    sectionId: "section3",
    content: "Working with MongoDB queries",
  },
];

const insertedUsers = [];
sampleUsers.forEach((user) => {
  const result = users.insertOne(user);
  insertedUsers.push({ _id: result.insertedId, ...user });
});

const insertedSkills = [];
sampleSkills.forEach((skill, index) => {
  const skillWithId = { ...skill, id: index + 1 };
  const result = skills.insertOne(skillWithId);
  insertedSkills.push(skillWithId);
});

const updatedSampleSections = sampleSections.map((section) => {
  const foundSkill = insertedSkills.find(
    (s) => s.skillName === section.skill.name
  );
  return { ...section, skill: foundSkill };
});

const insertedSections = [];
updatedSampleSections.forEach((section, index) => {
  const participantsWithObjectIds = section.participants.map((username) => {
    const foundUser = insertedUsers.find(
      (user) => user.gitUsername === username
    );
    return foundUser._id;
  });

  const sectionWithObjectIds = {
    ...section,
    id: index + 1,
    participants: participantsWithObjectIds,
  };
  const result = sections.insertOne(sectionWithObjectIds);
  insertedSections.push(sectionWithObjectIds);
});

sampleNotes.forEach((note) => {
  const foundUser = insertedUsers.find(
    (user) => user.gitUsername === note.userId
  );
  const foundSection = insertedSections.find(
    (section) => section.sectionName === note.sectionId
  );

  const noteWithObjectIds = {
    ...note,
    userId: foundUser._id,
    sectionId: foundSection._id,
  };
  notes.insertOne(noteWithObjectIds);
});

// Verification (Optional - to check if data was inserted correctly)
// db.users.find().pretty();
// db.skills.find().pretty();
// db.sections.find().pretty();
// db.notes.find().pretty();

print("Data inserted successfully!");
