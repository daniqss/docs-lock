import { User, CreateSkillRequest, Skill } from "../../__generated__/types.gen";
import client from "../client";

export const createSkill = (createSkillRequest: CreateSkillRequest) =>
  client.post("/skills", createSkillRequest).then((response) => response.data);

export const deleteSkill = (skillId: string) =>
  client.delete(`/skills/${skillId}`).then((response) => response.data);

export const getSkill = (skillId: string) =>
  client.get<Skill>(`/skills/${skillId}`).then((response) => response.data);

export const getSkills = async (): Promise<User[]> => {
  const response = await client.get<Skill[]>("/skills");
  return response.data;
};
