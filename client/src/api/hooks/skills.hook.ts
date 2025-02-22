import { useQuery, useMutation } from "@tanstack/react-query";
import {
  createSkill,
  deleteSkill,
  getSkill,
  getSkills,
} from "../client/services/skills";
import { CreateSkillRequest, Skill } from "../__generated__/types.gen";

export const useGetSkills = () => {
  return useQuery<Skill[], Error>({
    queryKey: ["skills"],
    queryFn: getSkills,
  });
};

export const useGet = (skillId: string) => {
  return useQuery<Skill, Error>({
    queryKey: ["skill", skillId],
    queryFn: async () => await getSkill(skillId),
  });
};

export const useCreateSkill = () => {
  return useMutation({
    mutationFn: (newSkill: CreateSkillRequest) => {
      return Promise.resolve(createSkill(newSkill));
    },
  });
};

export const useDeleteSkill = () => {
  return useMutation({
    mutationFn: (skillId: string) => {
      return Promise.resolve(deleteSkill(skillId));
    },
  });
};
